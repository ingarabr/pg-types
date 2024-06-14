package com.github.ingarabr.pg

import com.github.ingarabr.pg.Fixtures.IdWithLabel
import com.github.ingarabr.pg.PGObj.*
import doobie.Meta
import munit.FunSuite

class PGObjectStringConverterTest extends FunSuite {

  test("read nested case classes") {
    case class T1_1Row(id: IdWithLabel, txt: String) derives PGObjectStringConverter
    object T1_1Row {
      given Meta[T1_1Row] = PGObjectStringConverter[T1_1Row].toMetaObject("record")
    }

    val input = (Row(List(Row(List(Str("123"), Str("l_foo")), None), Str("foo")), None))

    assertEquals(
      summon[PGObjectStringConverter[T1_1Row]].read(input),
      Right(T1_1Row(IdWithLabel("123", "l_foo"), "foo"))
    )
  }

}
