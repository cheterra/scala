package common.db

//import portal.lib._

import scala.xml.{NodeSeq, Text}
import scala.collection.mutable.{ListBuffer}

import net.liftweb._

import mapper._
import util._
import http._
import common._


import scala.collection.mutable.{ListBuffer}

/** 
 * Search builder - the user has entered something in the search input field
 * Build a text snipped like 'column='search text'.
 * Check for hacking attempts in that text.
 * If the text contains '*' replace that with '%'
 * and create a 'like' clause: 'column like '....%....'
 * @param field the name of the column in the table
 * @param search the user has entered this text
 */
class BySearchBuilder[O <: Mapper[O]](field: String, search: String, params: Any*) {
  def get: QueryParam[O] = 
    BySql(str, IHaveValidatedThisSQL("frank", "2008-12-03"), is)
        
 /** make a part of a where clause for type String */
 def str: String = field + comp + "?"

 /** make the comparator of the part of the where clause. Either 'like' or '=' */
 def comp: String = {
   if (asStr.contains("%"))    " like "
   else                        " = "
 }

 /** replace '*' with '%' */
 lazy val asStr: String = search.replace("*", "%")

 /** return "" if empty of a part of a where clause for a string type */
 def is: String = 
     if (asStr.length == 0) ""
     else if (asStr.contains("--")) "" // silently ignore hacking attempt
     else if (asStr.contains(";")) "" // silently ignore hacking attempt
     else asStr
        
}
