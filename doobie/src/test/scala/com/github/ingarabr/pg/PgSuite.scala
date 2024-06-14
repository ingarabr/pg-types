package com.github.ingarabr.pg

import cats.effect.IO
import com.dimafeng.testcontainers.PostgreSQLContainer
import com.dimafeng.testcontainers.munit.TestContainerForAll
import doobie.Transactor
import munit.CatsEffectSuite
import org.testcontainers.utility.DockerImageName

abstract class PgSuite extends CatsEffectSuite with TestContainerForAll {

  override val containerDef: PostgreSQLContainer.Def = PostgreSQLContainer.Def(
    dockerImageName = DockerImageName.parse("postgres:15.1"),
    databaseName = "testcontainer-scala",
    username = "scala",
    password = "scala"
  )

  def withXa[A](runTest: Transactor[IO] => A): A = withContainers { (c: containerDef.Container) =>
    runTest(
      Transactor.fromDriverManager[IO](
        driver = "org.postgresql.Driver",
        url = c.jdbcUrl,
        user = c.username,
        password = c.password,
        logHandler = None
      )
    )
  }
}
