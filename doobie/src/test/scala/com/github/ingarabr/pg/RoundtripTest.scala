package com.github.ingarabr.pg

import cats.effect.IO
import doobie.{ConnectionIO, Meta}
import cats.syntax.all.*
import com.github.ingarabr.pg.Fixtures.{CNested, IdWithLabel}
import doobie.implicits.*

class RoundtripTest extends PgSuite {

  test("basic composite type") {
    val id = IdWithLabel("i1", "l1")
    withXa(xa =>
      for {
        _ <- List(Fixtures.idType, Fixtures.t1).combineAll.transact(xa)
        insId <- t1InsertId(id).transact(xa)
      } yield insId
    ).assertEquals(id)
  }

  test("basic composite type with ambiguous label value") {
    val id = IdWithLabel("i1", "{l1}")
    withXa(xa =>
      for {
        _ <- List(Fixtures.idType, Fixtures.t1).combineAll.transact(xa)
        insId <- t1InsertId(id).transact(xa)
      } yield insId
    ).assertEquals(id)
  }

  test("t1_1 query table row as a record") {
    val id = IdWithLabel("123", "l_foo")
    case class T1_1Row(id: IdWithLabel, txt: String) derives PGObjectStringConverter
    object T1_1Row {
      given Meta[T1_1Row] = PGObjectStringConverter[T1_1Row].toMetaObject("record")
    }
    withXa(xa =>
      for {
        _ <- List(Fixtures.idType, Fixtures.t1_1).combineAll.transact(xa)
        insId <- t1_1InsertId(id, "foo").transact(xa)
        row <- sql"select t from t1_1 t where id = $id limit 1".query[T1_1Row].unique.transact(xa)
      } yield row
    ).assertEquals(T1_1Row(id, "foo"))
  }

  test("t1_1 query table row as record with multiple columns") {
    val id = IdWithLabel("123", "l_foo")
    case class T1_1Row(id: IdWithLabel, txt: String) derives PGObjectStringConverter
    object T1_1Row {
      given Meta[T1_1Row] = PGObjectStringConverter[T1_1Row].toMetaObject("record")
    }
    withXa(xa =>
      for {
        _ <- List(Fixtures.idType, Fixtures.nestedType, Fixtures.t1_1).combineAll.transact(xa)
        insId <- t1_1InsertId(id, "foo").transact(xa)
        row <- sql"select t, 2 from t1_1 t where id = ${id} limit 1".query[(T1_1Row, Int)].unique.transact(xa)
      } yield row
    ).assertEquals((T1_1Row(id, "foo"), 2))
  }

  test("t3 nested composite types") {
    val id1 = IdWithLabel("1", "l_foo")
    val id2 = IdWithLabel("2", "l_foo")
    val nested = CNested(2, List(id1, id2), List("v1", "v2"))
    case class T1_1Row(id: IdWithLabel, txt: String) derives PGObjectStringConverter
    withXa(xa =>
      for {
        _ <- List(Fixtures.idType, Fixtures.nestedType, Fixtures.t3).combineAll.transact(xa)
        insId <- t3InsertCNested(nested).transact(xa)
        row <- sql"select t.obj from t3 t where id = ${insId} limit 1".query[CNested].unique.transact(xa)
      } yield row
    ).assertEquals(nested)
  }

  def t3InsertCNested(id: CNested): ConnectionIO[Long] =
    sql"""|insert into t3 (obj)
          |values ($id)
          |""".stripMargin.update.withUniqueGeneratedKeys[Long]("id")

  def t1InsertId(id: IdWithLabel): ConnectionIO[IdWithLabel] =
    sql"""|insert into t1 (id)
          |values ($id)
          |""".stripMargin.update.withUniqueGeneratedKeys[IdWithLabel]("id")

  def t1_1InsertId(id: IdWithLabel, txt: String): ConnectionIO[IdWithLabel] =
    sql"""|insert into t1_1 (id, txt)
            |values ($id, $txt)
            |""".stripMargin.update.withUniqueGeneratedKeys[IdWithLabel]("id")
}
