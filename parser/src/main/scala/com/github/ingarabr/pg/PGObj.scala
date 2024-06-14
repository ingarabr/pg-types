package com.github.ingarabr.pg

import cats.parse.{Parser as P, Parser0 as P0}
import PGObj.*

// https://www.postgresql.org/docs/current/rowtypes.html#ROWTYPES-IO-SYNTAX
enum PGObj {
  case Row(values: List[PGObj], typeName: Option[String] = None)
  case Arr(values: List[PGObj])
  case Null
  case Str(value: String)
}

object PGObj {

  def parse(input: String): Either[cats.parse.Parser.Error, PGObj] =
    Parser.parser.parseAll(input)

  object Parser {
    val objStart: P[Unit] = P.char('(')
    val objEnd: P[Unit] = P.char(')')
    val arrStart: P[Unit] = P.char('{')
    val arrEnd: P[Unit] = P.char('}')
    val sep: P[Unit] = P.char(',')
    val esc: P[Unit] = P.char('"')

    def unEscapedStr(stop: P[Unit]): P[PGObj] =
      P.until(sep | stop).string.map(Str.apply).withContext("unescaped string")

    def escapedStr(escCount: Int, stop: P[Unit]): P[PGObj] = {
      val escToken = esc.rep(escCount, escCount)
      val strSep = (escToken *> stop).backtrack | (esc *> sep).backtrack
      // todo: Might have some extra escaped string we need to handle.
      val parseEscaped: P0[String] = (escToken *> escToken.as('"') | P.anyChar).repUntilAs0[String](strSep)
      (escToken *> parseEscaped.map(Str.apply) <* escToken).withContext("escaped string")
    }

    val objNullValue: P0[PGObj] =
      P.peek(sep | objEnd).map(_ => Null).withContext("null value")

    val arrNullValue: P0[PGObj] = P.ignoreCase("null").map(_ => Null).withContext("null value")

    def rep0sep0Empty[A](data: P0[A], separator: P[Any]): P0[List[A]] =
      (data.? ~ (separator *> data).rep0).map { case (a, as) => a ++: as }

    private def escStartToken(level: Int, p: P[Unit]): P[Unit] =
      (if (level == 0) p else esc.rep(level, level) *> p).backtrack

    private def escEndToken(level: Int, p: P[Unit]): P[Unit] =
      if (level == 0) p else p <* esc.rep(level, level)

    def arr(level: Int, p: P0[PGObj]): P[PGObj] =
      escStartToken(level, arrStart) *> rep0sep0Empty(p, sep).map(Arr.apply) <* escEndToken(level, arrEnd)

    def arrNoEsc(p: P0[PGObj]): P[PGObj] =
      arrStart *> rep0sep0Empty(p, sep).map(Arr.apply) <* arrEnd


    def obj(level: Int, p: P0[PGObj]): P[PGObj] =
      escStartToken(level, objStart) *> rep0sep0Empty(p, sep).map(v => Row(v, None)) <* escEndToken(level, objEnd)

    // This isn't correct since it either can be null or one of the other base values.
    // However, since the values comes from PG we can probably assume it follows that pattern.
    def arrBase(escCount: Int): P0[PGObj] = escapedStr(escCount, arrEnd) | arrNullValue | unEscapedStr(arrEnd)
    def objBase(escCount: Int): P0[PGObj] = escapedStr(escCount, objEnd) | objNullValue | unEscapedStr(objEnd)

    val parser: P[PGObj] = {
      def levelParser(level: Int): P[PGObj] = {
        val rec = P.defer(levelParser(level + 1)).withContext(s"level-${level + 1}")
        val escCount = Math.pow(2, level).toInt
        val escRootCount = if (level == 0) 0 else Math.pow(2, level - 1).toInt
        val arrayP =
          arr(escRootCount, rec | arrBase(escCount)).withContext(s"array($level)") |
            arrNoEsc(rec | arrBase(escRootCount.max(1))).withContext(s"array-no-esc($level)")
        val objectP = obj(escRootCount, rec | objBase(escCount)).withContext(s"object($level)")
        arrayP | objectP
      }
      levelParser(0).withContext("level-0")
    }
  }

  extension (value: PGObj) {

    def renderString: String = {
      def innerRender(v: PGObj, esc: Int, parentIsArray: Boolean): String = {
        val wq = "\"" * esc
        val nextEsc = (esc * 2).max(1)
        def wrap(a: String, around: String): String = a ++ around ++ a
        val quoteWhenPresent = List(",", "(", ")", "{", "}", "\"", "\\", " ")
        v match {
          case Row(values, _) =>
            wrap(wq, values.map(v => innerRender(v, nextEsc, false)).mkString("(", ",", ")"))
          case Str(value) =>
            if (value.isEmpty) wrap(wq, "")
            else if (quoteWhenPresent.exists(value.contains(_)))
              wrap(wq, value.replace("\"", wq * 2))
            else value
          case Arr(values) =>
            if (parentIsArray) values.map(v => innerRender(v, esc, true)).mkString("{", ",", "}")
            else wrap(wq, values.map(v => innerRender(v, nextEsc, true)).mkString("{", ",", "}"))
          case Null => if (parentIsArray) "NULL" else ""
        }
      }
      innerRender(value, 0, false)
    }

    def renderSql: String = {
      def inner(v: PGObj, parentIsArray: Boolean): String =
        v match {
          case PGObj.Row(values, typeName) =>
            values.map(inner(_, false)).mkString("ROW(", ",", ")") ++ typeName.fold("")(n => s"::$n")
          case PGObj.Arr(values) => values.map(inner(_, true)).mkString("ARRAY[", ",", "]")
          case PGObj.Null        => if (parentIsArray) "NULL" else ""
          case PGObj.Str(value)  => "'" ++ value.replace("'", "''") ++ "'"
        }
      inner(value, false)
    }
  }

}
