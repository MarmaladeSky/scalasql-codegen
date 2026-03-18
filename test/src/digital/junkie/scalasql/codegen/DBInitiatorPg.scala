package digital.junkie.scalasql.codegen

import java.sql.DriverManager
import java.time.Instant
import scala.util.Random

case class DbCredentials(
    host: String,
    port: Int,
    user: String,
    password: String,
    database: String
) {
  def jdbcUrl: String = s"jdbc:postgresql://$host:$port/$database"
}

class DBInitiatorPg {

  def create(creds: DbCredentials): DbCredentials = {
    val dbName = s"test_db_${Instant.now().getEpochSecond}_${randomChars(5)}"
    val conn =
      DriverManager.getConnection(creds.jdbcUrl, creds.user, creds.password)
    try {
      val stmt = conn.createStatement()
      stmt.execute(s"CREATE DATABASE $dbName")
      stmt.close()
    } finally {
      conn.close()
    }
    creds.copy(database = dbName)
  }

  private def randomChars(n: Int): String = {
    Random.alphanumeric.take(n).mkString.toLowerCase
  }

}
