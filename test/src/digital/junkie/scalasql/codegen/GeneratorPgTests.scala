package digital.junkie.scalasql.codegen

import utest.*
import java.sql.DriverManager
import scala.meta.XtensionSyntax

object GeneratorPgTests extends TestSuite {

  private def credsFromEnv: DbCredentials = DbCredentials(
    host = sys.env.getOrElse("PG_HOST", "localhost"),
    port = sys.env.getOrElse("PG_PORT", "5432").toInt,
    user = sys.env.getOrElse("PG_USER", "postgres"),
    password = sys.env.getOrElse("PG_PASSWORD", "postgres"),
    database = sys.env.getOrElse("PG_DATABASE", "postgres")
  )

  def tests = Tests {
    test("generates case class per table") {
      val testCreds = new DBInitiatorPg().create(credsFromEnv)
      val conn = DriverManager.getConnection(
        testCreds.jdbcUrl,
        testCreds.user,
        testCreds.password
      )
      try {
        val stmt = conn.createStatement()
        stmt.execute("CREATE SCHEMA test_schema")
        stmt.execute(
          "CREATE TABLE test_schema.test_table (id SERIAL PRIMARY KEY)"
        )
        stmt.close()

        val generator = Generator.impl()

        val code = generator.generate(conn, "test_schema", "com.example") match {
          case Right(source) => source.syntax
          case Left(err)     => throw new RuntimeException(err)
        }

        assert(code.contains("package com.example"))
        assert(code.contains("case class TestTable(id: Int)"))
        assert(
          code.contains("object TestTable extends SimpleTable[TestTable]")
        )
      } finally {
        conn.close()
      }
    }
  }

}
