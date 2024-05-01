package com.github.ingarabr.pg

import cats.syntax.all.*
import cats.Show
import cats.data.NonEmptyList
import com.github.ingarabr.pg
import doobie.Put
import doobie.util.Get
import doobie.util.meta.Meta
import magnolia1.{CaseClass, ProductDerivation}
import org.postgresql.util.PGobject

import scala.reflect.ClassTag

trait NamedPGObj {
  def pgTypeName: String
}

trait PGObjectStringConverter[A] { self =>
  def write(a: A): PGObj
  def read(obj: PGObj): Either[String, A]

  def emap[B](f: B => A, t: A => Either[String, B]): PGObjectStringConverter[B] =
    new PGObjectStringConverter[B] {
      def write(b: B): PGObj = self.write((f(b)))
      def read(obj: PGObj): Either[String, B] = self.read(obj).flatMap(t)
    }

  def toMetaObject(typeName: String)(using PGObjectStringConverter[A]): Meta[A] = {
    given Show[PGobject] = _.getValue
    Meta.Advanced
      .other[PGobject](typeName)
      .tiemap[A](pgObj =>
        val res = PGObj
          .parse(pgObj.getValue)
          .fold(err => Left(err.toString()), PGObjectStringConverter[A].read)

        pgObj.getValue
        println("TADA: " ++ res.toString)
        res
      )(a => {
        val o = new PGobject()
        o.setType(typeName)
        o.setValue(PGObjectStringConverter[A].write(a).renderString)
        o
      })
  }

  def toMetaArray[B >: A: ClassTag](
      typeName: String
  )(using PGObjectStringConverter[B]): Meta[Array[B]] = {
    given Show[Array[Object]] = _.mkString(",")
    val get = Get.Advanced
      .array[Object](NonEmptyList.of(typeName))
      .temap(_.toList.traverse {
        // workaround for ClassCastException on arrays
        case o: PGobject => Right(o.getValue)
        case other       => Left(s"expected pg-object but got ${other.getClass}")
      })
      .temap(strArr =>
        strArr.traverse(str => PGObj.parse(str).leftMap(_.toString()).flatMap(PGObjectStringConverter[B].read))
      )
      .map(_.toArray)
    val put = Put.Advanced
      .array[String](NonEmptyList.of(typeName), typeName)
      .contramap[Array[B]](arr => arr.map(a => PGObjectStringConverter[B].write(a).renderString))
    new Meta[Array[B]](get, put)
  }

}

object PGObjectStringConverter extends ProductDerivation[PGObjectStringConverter] {

  def apply[A](using PGObjectStringConverter[A]): PGObjectStringConverter[A] =
    summon[PGObjectStringConverter[A]]

  override def join[T](ctx: CaseClass[Typeclass, T]): Typeclass[T] =
    new PGObjectStringConverter[T]() {

      override def write(a: T): PGObj = {
        val name = a match {
          case n: NamedPGObj => Some(n.pgTypeName)
          case _             => None
        }
        PGObj.Row(ctx.parameters.map(param => param.typeclass.write(param.deref(a))).toList, name)
      }

      override def read(obj: PGObj): Either[String, T] = {
        obj match {
          case PGObj.Row(values, _) =>
            val vArr = values.toArray
            ctx.constructEither(param => param.typeclass.read(vArr(param.index))).leftMap(_.mkString(", "))
          case _ => Left("value must be an object")
        }
      }
    }

  given [A](using PGObjectStringConverter[A]): PGObjectStringConverter[Option[A]] with {
    def read(obj: PGObj): Either[String, Option[A]] = obj match {
      case PGObj.Null => Right(None)
      case v                   => PGObjectStringConverter[A].read(v).map(Some(_))
    }

    def write(a: Option[A]): PGObj =
      a.fold(PGObj.Null)(PGObjectStringConverter[A].write)

  }

  given [A](using PGObjectStringConverter[A]): PGObjectStringConverter[List[A]] with {
    def read(obj: PGObj): Either[String, List[A]] = obj match {
      case PGObj.Arr(value) =>
        value.traverse(v => PGObjectStringConverter[A].read(v))
      case v => Left("expected an array")
    }

    def write(a: List[A]): PGObj =
      PGObj.Arr(a.map(PGObjectStringConverter[A].write))

  }

  given [A: ClassTag](using PGObjectStringConverter[List[A]]): PGObjectStringConverter[Array[A]] =
    PGObjectStringConverter[List[A]].emap(_.toList, l => Right(l.toArray))

  given PGObjectStringConverter[String] with {
    def read(obj: PGObj): Either[String, String] = obj match {
      case PGObj.Str(value) => Right(value)
      case _                         => Left(s"unexpected type")
    }
    def write(a: String): PGObj = PGObj.Str(a)
  }

  given PGObjectStringConverter[Int] =
    PGObjectStringConverter[String].emap(_.toString, _.toIntOption.toRight("parse error"))

  given PGObjectStringConverter[Long] =
    PGObjectStringConverter[String].emap(_.toString, _.toLongOption.toRight("parse error"))

  given PGObjectStringConverter[Short] =
    PGObjectStringConverter[String].emap(_.toString, _.toShortOption.toRight("parse error"))

}
