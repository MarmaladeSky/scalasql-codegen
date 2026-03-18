package digital.junkie.scalasql.codegen

trait NamingConvention {

  def apply(origin: String): String

}

object NamingConvention {

  val NoChange: NamingConvention = new NamingConvention {
    def apply(origin: String): String = origin
  }

  val CamelCase: NamingConvention = new NamingConvention {
    def apply(origin: String): String =
      origin.toLowerCase.split("_").map(_.capitalize).mkString
  }

  val camelCase: NamingConvention = new NamingConvention {
    def apply(origin: String): String = {
      val parts = origin.toLowerCase.split("_")
      (parts.head +: parts.tail.map(_.capitalize)).mkString
    }
  }

  val snake_case: NamingConvention = new NamingConvention {
    def apply(origin: String): String =
      origin
        .replaceAll("([A-Z]+)([A-Z][a-z])", "$1_$2")
        .replaceAll("([a-z\\d])([A-Z])", "$1_$2")
        .replaceAll("[\\s\\-]+", "_")
        .toLowerCase
  }

}
