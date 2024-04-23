package com.github.ingarabr.pg

import cats.data.Validated
import cats.syntax.all.*
import com.github.ingarabr.pg.PGObj.*
import munit.*

class PGObjTest extends FunSuite {
  case class TestCase(
      id: Int,
      ast: PGObj,
      sqlOutput: String,
      sql: String
  )
  private val testCases = List(
    TestCase(
      id = 1,
      ast = Row(List(Str("s1"))),
      sqlOutput = "(s1)",
      sql = "ROW('s1')"
    ),
    TestCase(
      id = 2,
      ast = Row(List(Str("s"), Str("NULL"))),
      sqlOutput = "(s,NULL)",
      sql = "ROW('s','NULL')"
    ),
    TestCase(
      id = 3,
      ast = Row(List(Str("s1"), Str("s2"))),
      sqlOutput = "(s1,s2)",
      sql = "ROW('s1','s2')"
    ),
    TestCase(
      id = 4,
      ast = Row(List(Str("s1"), Arr(List(Str("a1"))))),
      sqlOutput = "(s1,{a1})",
      sql = "ROW('s1',ARRAY['a1'])"
    ),
    TestCase(
      id = 5,
      ast = Row(List(Str("s1"), Arr(List(Str("a1"), Str("a2"))))),
      sqlOutput = "(s1,\"{a1,a2}\")",
      sql = "ROW('s1',ARRAY['a1','a2'])"
    ),
    TestCase(
      id = 6,
      ast = Row(List(Str("s1"), Arr(List(Str("a1"), Null, Str("a2"))))),
      sqlOutput = "(s1,\"{a1,NULL,a2}\")",
      sql = "ROW('s1',ARRAY['a1',NULL,'a2'])"
    ),
    TestCase(
      id = 7,
      ast = Row(List(Str("s1"), Row(List(Str("o1"), Str("o2"))))),
      sqlOutput = "(s1,\"(o1,o2)\")",
      sql = "ROW('s1',ROW('o1','o2'))"
    ),
    TestCase(
      id = 8,
      ast = Row(List(Row(List(Row(List(Row(List(Str("nested4"))))))))),
      sqlOutput = "(\"(\"\"(\"\"\"\"(nested4)\"\"\"\")\"\")\")",
      sql = "ROW(ROW(ROW(ROW('nested4'))))"
    ),
    TestCase(
      id = 9,
      ast = Row(List(Str("esc\""), Str("\"esc"))),
      sqlOutput = "(\"esc\"\"\",\"\"\"esc\")",
      sql = "ROW('esc\"','\"esc')"
    ),
    TestCase(
      id = 10,
      ast = Arr(List(Str("s1"), Str("s2"))),
      sqlOutput = "{s1,s2}",
      sql = "ARRAY['s1','s2']"
    ),
    TestCase(
      id = 11,
      ast = Arr(List(Arr(List(Str("a1-1"), Str("a1-2"))), Arr(List(Str("a2-1"), Str("a2-2"))))),
      sqlOutput = "{{a1-1,a1-2},{a2-1,a2-2}}",
      sql = "ARRAY[ARRAY['a1-1','a1-2'],ARRAY['a2-1','a2-2']]"
    ),
    TestCase(
      id = 12,
      ast = Arr(List(Arr(List(Str("a1\""), Null)))),
      sqlOutput = "{{a1\",NULL}}",
      sql = "ARRAY[ARRAY['a1\"',NULL]]"
    ),
    TestCase(
      id = 13,
      ast = Arr(List(Str("a1"), Null)),
      sqlOutput = "{a1,NULL}",
      sql = "ARRAY['a1',NULL]"
    ),
    TestCase(
      id = 14,
      ast = Row(List(Row(List(Row(List(Row(List(Str("esc\"me"))))))))),
      sqlOutput =
        "(\"(\"\"(\"\"\"\"(\"\"\"\"\"\"\"\"esc\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"me\"\"\"\"\"\"\"\")\"\"\"\")\"\")\")",
      sql = "ROW(ROW(ROW(ROW('esc\"me'))))"
    )
  )

  test("ast -> string -> ast") {
    testCases
      .filterNot(tc =>
        List(
          12 // quoted array
        ).contains(tc.id)
      )
      .traverse(tc => {
        val str = tc.ast.renderString
        PGObj
          .parse(str)
          .fold(
            err => s"${tc.id}: input: $str err: ${err.toString()}".invalidNel,
            pObj => if (pObj == tc.ast) ().validNel else s"${tc.id}: ${tc.ast} != $pObj :: str:$str".invalidNel
          )
      }) match {
      case Validated.Valid(a) => ()
      case Validated.Invalid(e) =>
        fail(e.mkString_("\n"))
    }
  }

  test("ast -> sql-output") {
    testCases
      .filterNot(tc =>
        List(
          4, // to many quotes in array. one element
          11 // to many quotes in array. nested array
//          12 // to many quotes in array. nested array
        ).contains(tc.id)
      )
      .foreach { tc =>
        assertEquals(tc.ast.renderString, tc.sqlOutput, s"tc: ${tc.id}")
      }
  }

  test("ast -> sql") {
    testCases
      .foreach { tc =>
        assertEquals(tc.ast.renderSql, tc.sql, s"tc: ${tc.id}")
      }
  }

}
