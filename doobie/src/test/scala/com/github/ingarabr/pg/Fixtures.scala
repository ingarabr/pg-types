package com.github.ingarabr.pg

import doobie.{ConnectionIO, Fragment, Meta}
import doobie.syntax.*
import doobie.implicits.*

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

  val nestedType: ConnectionIO[Int] =
    createIfNotExist(
      sql"""|CREATE TYPE c_nested AS (
            |    num   integer,
            |    ids   id_with_label_v1[],
            |    strs  TEXT[]
            |)
            |""".stripMargin
    ).update.run

  val t1: ConnectionIO[Int] =
    sql"""|CREATE TABLE IF NOT EXISTS t1 (
          |  id   id_with_label_v1
          |)
          |""".stripMargin.update.run

  val t1_1: ConnectionIO[Int] =
    sql"""|CREATE TABLE IF NOT EXISTS t1_1 (
          |  id   id_with_label_v1,
          |  txt  TEXT
          |)
          |""".stripMargin.update.run


  val t2: ConnectionIO[Int] =
    sql"""|CREATE TABLE IF NOT EXISTS t2 (
          |  id   serial,
          |  ids  id_with_label_v1[]
          |)
          |""".stripMargin.update.run

  val t3: ConnectionIO[Int] =
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
