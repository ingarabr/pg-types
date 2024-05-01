package com.github.ingarabr.pg

import munit.FunSuite
import doobie.*
import doobie.implicits.*
import doobie.postgres.*
import doobie.postgres.implicits.*
import cats.effect.IO
import cats.*
import cats.data.Chain
import cats.syntax.all.*
import cats.effect.unsafe.implicits.global
import com.github.ingarabr.pg.Fixtures.{CNested, IdWithLabel}
import doobie.util.fragment.Elem.Arg
import org.postgresql.util.PGobject

class RoundtripTest extends FunSuite {

  // docker run --name pg-composite -e POSTGRES_PASSWORD=password -e POSTGRES_USER=postgres -p 5432:5432 -d postgres
  val xa = Transactor.fromDriverManager[IO](
    driver = "org.postgresql.Driver",
    url = "jdbc:postgresql:",
    user = "postgres",
    password = "password",
    logHandler = None
  )

  test("meheheheh") {
    val r =  (for {
      str <- sql""" select row(row('(o1)'))""".query[String].unique.transact(xa)
      bs <- sql""" select row(row('(o1)'))""".query[Array[Byte]].unique.transact(xa)
//      bs <- sql""" select row(row('(o1)'))""".query[PGobject].unique.transact(xa)
    } yield s"""|Out:
                |- $str
                |- ${new String(bs)}
                |""".stripMargin
      ).unsafeRunSync()
    println(r)
  }

  def insertId(id: IdWithLabel) =
    sql"""|insert into t1 (id)
          |values ($id)
          |""".stripMargin.update.withUniqueGeneratedKeys[IdWithLabel]("id")

  def insertIds(ids: List[IdWithLabel]) =
    sql"""|insert into t2 (ids)
          |values (${ids.toArray})
          |""".stripMargin.update
      .withUniqueGeneratedKeys[Long]("id")

  def insertIdsAndRet(ids: List[IdWithLabel]) =
    sql"""|insert into t2 (ids)
          |values (${ids.toArray})
          |""".stripMargin.update
      .withUniqueGeneratedKeys[(Long, Array[IdWithLabel])]("id", "ids")

  def queryIds(i: Long) =
    sql"""|select ids
          |from t2
          |where id = $i
          |""".stripMargin
      .query[Array[IdWithLabel]]
      .unique

  test("parse me") {
    (for {
      _ <- List(Fixtures.idType, Fixtures.nestedType, Fixtures.t1, Fixtures.t2, Fixtures.t3).combineAll.transact(xa)
      singleObj <- insertId(IdWithLabel("1", "fo\n\"o")).transact(xa)
      arrayWithId <- insertIds(List(IdWithLabel("1", "a"), IdWithLabel("1", "b"))).transact(xa)
      arrayWithId2 <- insertIdsAndRet(List(IdWithLabel("1", "a"), IdWithLabel("1", "b"))).transact(xa)
      ids <- queryIds(arrayWithId).transact(xa).map(_.toList)
      _ <- IO.println(
        s"""|-- inserts with generated:
            | singleObj: ${singleObj}
            | arrayWithId: ${arrayWithId}
            | arrayWithId2: ${arrayWithId2._1} ${arrayWithId2._2.toList}
            | 
            |-- Query:
            | id: ${ids}
            |""".stripMargin
      )
    } yield ())
      .unsafeRunSync()
  }

  test("nested") {
    def fromId(i: Long) =
      sql"""|select obj
            |from t3
            |where id = $i
            |""".stripMargin
        .query[CNested]
        .unique

    def fromIdArr(i: Long) =
      sql"""|select obj
            |from t3
            |where id = $i
            |""".stripMargin
        .query[Array[Byte]]
        .unique
    def fromQ =
      sql"""select row('(o1)')"""
        .query[CNested]
        .unique

//    def insertCNestedSimple(id: CNested, ids: List[CNested]) =
//      sql"""|insert into t3 (obj)
//            |values (${id})
//            |""".stripMargin.update
//        .withUniqueGeneratedKeys[Long]("id")

    def insertCNested(id: CNested, ids: List[CNested]) = {
      val foo =
        sql"""|insert into t3 (obj)
                |values ($id)
                |""".stripMargin
      println("SQL: " ++ foo.internals.sql ++ "\n" ++ foo.internals.arg.toString)

//        println("ARG: " ++ foo.internals.arg.asInstanceOf[Chain[Arg[Any]]].map(_._2.toString).toString)
      foo.update
        .withUniqueGeneratedKeys[Long]("id")
    }

    (for {
      _ <- List(Fixtures.idType, Fixtures.nestedType, Fixtures.t1, Fixtures.t2, Fixtures.t3).combineAll.transact(xa)
//      singleObj <- insertCNested(
//        CNested(1, List(IdWithLabel("1", "foo")), List("one", "two")),
//        List(
//          CNested(2, List(IdWithLabel("2", "f0o")), List("one", "two")),
//          CNested(3, List(IdWithLabel("3", "foo")), List("one", "two"))
//        )
//      ).transact(xa)
//      _ <- IO.println(s"Query ${singleObj}")
//      res <- fromId(singleObj).transact(xa)
//      res3 <- fromQ.transact(xa)
      res <- fromId(1).transact(xa)
      res2 <- fromIdArr(1).transact(xa)

      _ <- IO.println(
        s"""|-- inserts with generated:
            |
            |-- Query:
            |$res
            |
            |Res2:
            |${new String(res2)}
            |""".stripMargin
      )
    } yield ())
      .unsafeRunSync()
  }

  test("meh") {
    println(
      PGObjectStringConverter[CNested]
        .write(CNested(1, List(IdWithLabel("1", "foo")), List("one", "two")))
        .renderSql
    )

  }

}

object Fixtures {
  def createIfNotExist(typeFrag: Fragment) =
    sql"""|DO $$$$ BEGIN
          |$typeFrag;
          |EXCEPTION
          |    WHEN duplicate_object THEN null;
          |END $$$$;
          |""".stripMargin

  val idType =
    createIfNotExist(
      sql"""|CREATE TYPE id_with_label_v1 AS (
            |    id    TEXT,
            |    label TEXT
            |)
            |""".stripMargin
    ).update.run

  val nestedType =
    createIfNotExist(
      sql"""|CREATE TYPE c_nested AS (
            |    num   integer,
            |    ids   id_with_label_v1[],
            |    strs  TEXT[]
            |)
            |""".stripMargin
    ).update.run

  val t1 =
    sql"""|CREATE TABLE IF NOT EXISTS t1 (
          |  id   id_with_label_v1
          |)
          |""".stripMargin.update.run

  val t2 =
    sql"""|CREATE TABLE IF NOT EXISTS t2 (
          |  id   serial,
          |  ids  id_with_label_v1[]
          |)
          |""".stripMargin.update.run

  val t3 =
    sql"""|CREATE TABLE IF NOT EXISTS t3 (
          |  id   serial,
          |  obj  c_nested,
          |  arr  c_nested[]
          |)
          |""".stripMargin.update.run

  case class IdWithLabel(id: String, label: String) extends NamedPGObj derives PGObjectStringConverter {
    override def pgTypeName: String = "id_with_label_v1"
  }

  object IdWithLabel {
    given Meta[IdWithLabel] = PGObjectStringConverter[IdWithLabel].toMetaObject("id_with_label_v1")
    given Meta[Array[IdWithLabel]] = PGObjectStringConverter[IdWithLabel].toMetaArray("id_with_label_v1")
  }

  case class CNested(num: Int, ids: List[IdWithLabel], strs: List[String]) extends NamedPGObj
      derives PGObjectStringConverter {
    override def pgTypeName: String = "c_nested"
  }
  object CNested {
    given Meta[CNested] = PGObjectStringConverter[CNested].toMetaObject("c_nested")
    given Meta[Array[CNested]] = PGObjectStringConverter[CNested].toMetaArray("c_nested")
  }
}
