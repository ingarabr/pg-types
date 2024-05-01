ThisBuild / scalaVersion := "3.3.1"
ThisBuild / organization := "com.github.ingarabr.pg"

lazy val parser = (project in file("parser"))
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-parse" % "0.3.9",
      "org.scalameta" %% "munit" % "0.7.29" % Test
    )
  )


lazy val doobie = (project in file("doobie"))
  .dependsOn(parser)
  .settings(
    libraryDependencies ++= Seq(
      "org.tpolecat" %% "doobie-postgres" % "1.0.0-RC4",
      "com.softwaremill.magnolia1_3" %% "magnolia" % "1.3.5",
      "org.scalameta" %% "munit" % "0.7.29" % Test
    )
  )
