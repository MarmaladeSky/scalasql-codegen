package digital.junkie.scalasql.codegen

import java.sql.{Connection, DatabaseMetaData, Types}

case class ColumnInfo(name: String, scalaType: String, nullable: Boolean)

trait SchemaReader {

  def readTables(conn: Connection, schema: String): List[String]

  def readColumns(
      conn: Connection,
      schema: String,
      table: String
  ): List[ColumnInfo]

}

object SchemaReader {

  def forConnection(conn: Connection): Either[String, SchemaReader] =
    conn.getMetaData.getDatabaseProductName.toLowerCase match {
      case p if p.contains("sqlite")     => Right(SQLite)
      case p if p.contains("postgresql") => Right(Postgres)
      case p                             => Left(s"Unsupported database: $p")
    }

  object Postgres extends SchemaReader {

    def readTables(conn: Connection, schema: String): List[String] = {
      val rs = conn.getMetaData.getTables(null, schema, "%", Array("TABLE"))
      val buf = List.newBuilder[String]
      while (rs.next()) buf += rs.getString("TABLE_NAME")
      rs.close()
      buf.result()
    }

    def readColumns(
        conn: Connection,
        schema: String,
        table: String
    ): List[ColumnInfo] = {
      val rs = conn.getMetaData.getColumns(null, schema, table, "%")
      val buf = List.newBuilder[ColumnInfo]
      while (rs.next()) {
        val name = rs.getString("COLUMN_NAME")
        val scalaType = jdbcTypeToScala(rs.getInt("DATA_TYPE"))
        val nullable = rs.getInt("NULLABLE") == DatabaseMetaData.columnNullable
        buf += ColumnInfo(name, scalaType, nullable)
      }
      rs.close()
      buf.result()
    }

    private def jdbcTypeToScala(jdbcType: Int): String = jdbcType match {
      case Types.VARCHAR | Types.CHAR | Types.LONGVARCHAR | Types.NVARCHAR |
          Types.NCHAR =>
        "String"
      case Types.INTEGER                                   => "Int"
      case Types.BIGINT                                    => "Long"
      case Types.SMALLINT                                  => "Short"
      case Types.TINYINT                                   => "Byte"
      case Types.BOOLEAN | Types.BIT                       => "Boolean"
      case Types.DOUBLE                                    => "Double"
      case Types.FLOAT | Types.REAL                        => "Float"
      case Types.DECIMAL | Types.NUMERIC                   => "BigDecimal"
      case Types.DATE                                      => "LocalDate"
      case Types.TIMESTAMP | Types.TIMESTAMP_WITH_TIMEZONE => "LocalDateTime"
      case _                                               => "String"
    }

  }

  object SQLite extends SchemaReader {

    def readTables(conn: Connection, schema: String): List[String] = {
      val rs = conn.getMetaData.getTables(null, null, "%", Array("TABLE"))
      val buf = List.newBuilder[String]
      while (rs.next()) buf += rs.getString("TABLE_NAME")
      rs.close()
      buf.result()
    }

    def readColumns(
        conn: Connection,
        schema: String,
        table: String
    ): List[ColumnInfo] = {
      val stmt = conn.createStatement()
      val rs = stmt.executeQuery(s"PRAGMA table_info($table)")
      val buf = List.newBuilder[ColumnInfo]
      while (rs.next()) {
        val name = rs.getString("name")
        val scalaType = readColumnType(rs.getString("type"))
        val notNull = rs.getInt("notnull") == 1
        val isPk = rs.getInt("pk") > 0
        buf += ColumnInfo(name, scalaType, nullable = !notNull && !isPk)
      }
      rs.close()
      stmt.close()
      buf.result()
    }

    def readColumnType(typeName: String): String = {
      val t = typeName.toUpperCase.trim
      if (t.contains("BIGINT")) "Long"
      else if (t.contains("INT")) "Int"
      else if (t.contains("CHAR") || t.contains("TEXT") || t.contains("CLOB"))
        "String"
      else if (t.contains("DATETIME") || t.contains("TIMESTAMP"))
        "LocalDateTime"
      else if (t.contains("DATE")) "LocalDate"
      else if (t.contains("BOOL")) "Boolean"
      else if (
        t.contains("REAL") || t.contains("DOUBLE") || t.contains("FLOAT")
      ) "Double"
      else if (t.contains("NUMERIC") || t.contains("DECIMAL")) "BigDecimal"
      else "String"
    }

  }

}
