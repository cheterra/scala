package taf
package snippet

import net.liftweb._
import util._
import http._
import mapper._
import taf.db._

/**
 * TAF field
 */
class TafField (dbField: BaseField, value: String) {
  /** make a part of a where clause for type String */ 
  def str: String = " and " + value + comp + "'" + asStr + "'"
  /** make the comparator of the part of the where clause. Either 'like' or '=' */
  def comp: String = {
    if (asStr.contains("%"))    " like "
    else                        " = "
  }

  /** replace '*' with '%' */
  def asStr: String = dbField.get.asInstanceOf[String].replace("*", "%")

  /** return "" if empty of a part of a where clause for a string type */
  def get: String = dbField.get match {
    case d:String => { 
      if (asStr.length == 0) ""
      else if (asStr.contains("--")) "" // silently ignore hacking attempt
        else str
    } 
    case _ => println("Value is not a String"); ""
  }

  def getValue = dbField.get.asInstanceOf[String]
}



