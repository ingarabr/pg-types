ThisBuild / scalaVersion := "3.3.1"
ThisBuild / organization := "com.github.ingarabr.pg"

val munit = "1.0.0"
val testContainers = "0.41.3"

lazy val parser = (project in file("parser"))
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-parse" % "0.3.9",
      "org.scalameta" %% "munit" % munit % Test
    )
  )

lazy val doobie = (project in file("doobie"))
  .dependsOn(parser)
  .settings(
    libraryDependencies ++= Seq(
      "org.tpolecat" %% "doobie-postgres" % "1.0.0-RC4",
      "com.softwaremill.magnolia1_3" %% "magnolia" % "1.3.5",
      "org.scalameta" %% "munit" % munit % Test,
      "org.typelevel" %% "munit-cats-effect" % "2.0.0" % Test,
      "com.dimafeng" %% "testcontainers-scala-munit" % testContainers % Test,
      "com.dimafeng" %% "testcontainers-scala-postgresql" % testContainers % Test
    ),
    Test / fork := true
  )
