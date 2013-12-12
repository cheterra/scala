package taf
package lib


object Db {

      def toSql(value: Any): String = {
        value match {
          case x:List[Any] => Bib.join(x, ", ")  // TODO remove dupes
          case _  => String.valueOf(value)
        }
      }
}