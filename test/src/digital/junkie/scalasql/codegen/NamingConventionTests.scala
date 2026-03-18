package digital.junkie.scalasql.codegen

import utest.*

object NamingConventionTests extends TestSuite {

  def tests = Tests {

    test("NoChange") {
      assert(NamingConvention.NoChange("my_table_name") == "my_table_name")
      assert(NamingConvention.NoChange("MyTable") == "MyTable")
      assert(NamingConvention.NoChange("myColumn") == "myColumn")
    }

    test("CamelCase") {
      assert(NamingConvention.CamelCase("my_table_name") == "MyTableName")
      assert(NamingConvention.CamelCase("table") == "Table")
      assert(NamingConvention.CamelCase("MY_TABLE_NAME") == "MyTableName")
    }

    test("camelCase") {
      assert(NamingConvention.camelCase("my_column_name") == "myColumnName")
      assert(NamingConvention.camelCase("column") == "column")
      assert(NamingConvention.camelCase("MY_COLUMN_NAME") == "myColumnName")
    }

    test("snake_case") {
      assert(NamingConvention.snake_case("MyTableName") == "my_table_name")
      assert(NamingConvention.snake_case("myColumnName") == "my_column_name")
      assert(NamingConvention.snake_case("XMLParser") == "xml_parser")
      assert(NamingConvention.snake_case("my_table_name") == "my_table_name")
    }

  }

}
