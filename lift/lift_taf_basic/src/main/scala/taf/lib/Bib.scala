package taf
package lib


object Bib {

      def join(list: List[Any], sep: String): String = {
         val buf = new StringBuilder()
         for (it <- list) {
           if (!buf.isEmpty) buf.append(sep)
           buf.append("" + it)
         }
         buf.toString
      }
}