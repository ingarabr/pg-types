ThisBuild / scalaVersion := "3.3.1"
ThisBuild / organization := "com.github.ingarabr.pg"

lazy val parser = (project in file("parser"))
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-parse" % "0.3.9",
      "org.scalameta" %% "munit" % "0.7.29" % Test
    )
  )
