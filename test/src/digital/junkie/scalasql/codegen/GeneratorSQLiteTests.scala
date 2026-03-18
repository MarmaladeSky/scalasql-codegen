package digital.junkie.scalasql.codegen

import utest.*
import java.sql.DriverManager
import java.nio.file.Files
import scala.meta.XtensionSyntax

object GeneratorSQLiteTests extends TestSuite {

  def tests = Tests {
    test("generates case class per table") {
      val dbFile = Files.createTempFile("scalasql_codegen_test_", ".db")
      val conn =
        DriverManager.getConnection(s"jdbc:sqlite:${dbFile.toAbsolutePath}")
      try {
        val stmt = conn.createStatement()
        stmt.execute("CREATE TABLE test_table (id INTEGER PRIMARY KEY)")
        stmt.close()

        val code = Generator.impl().generate(conn, null, "com.example") match {
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
        Files.deleteIfExists(dbFile)
      }
    }
  }

}
