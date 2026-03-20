package digital.junkie.scalasql.codegen

import java.sql.Connection
import scala.meta.*

trait Generator {

  def generate(
      conn: Connection,
      schema: String,
      pkg: String
  ): Either[String, Source]

}

object Generator {

  def impl(
      tableNaming: NamingConvention = NamingConvention.CamelCase,
      columnNaming: NamingConvention = NamingConvention.camelCase
  ): Generator = new Generator {

    def generate(
        conn: Connection,
        schema: String,
        pkg: String
    ): Either[String, Source] = {
      SchemaReader.forConnection(conn).map { reader =>
        val tables = reader.readTables(conn, schema)
        val tableColumns =
          tables.map(t => (t, reader.readColumns(conn, schema, t)))
        val usedTypes = tableColumns.flatMap(_._2.map(_.scalaType)).toSet

        val imports: List[Stat] = List(
          Some(q"import scalasql.SimpleTable"),
          Option.when(usedTypes.contains("LocalDate"))(
            q"import java.time.LocalDate"
          ),
          Option.when(usedTypes.contains("LocalDateTime"))(
            q"import java.time.LocalDateTime"
          )
        ).flatten

        val classStats: List[Stat] =
          tableColumns.flatMap { case (tableName, columns) =>
            val tableTypeName = tableNaming(tableName)
            val className = Type.Name(tableTypeName)
            val classTermName = Term.Name(tableTypeName)
            val params: List[Term.Param] = columns.map { col =>
              val baseType = Type.Name(col.scalaType)
              val tpe: Type =
                if (col.nullable) t"Option[$baseType]" else baseType
              Term
                .Param(Nil, Term.Name(columnNaming(col.name)), Some(tpe), None)
            }
            List(
              q"case class $className(..$params)",
              q"object $classTermName extends SimpleTable[$className]"
            )
          }

        Source(List(Pkg(toRef(pkg), imports ++ classStats)))
      }
    }

    private def toRef(pkg: String): Term.Ref =
      pkg.split("\\.").toList match {
        case head :: tail =>
          tail.foldLeft[Term.Ref](Term.Name(head))((acc, p) =>
            Term.Select(acc, Term.Name(p))
          )
        case Nil => Term.Name(pkg)
      }

  }

}
