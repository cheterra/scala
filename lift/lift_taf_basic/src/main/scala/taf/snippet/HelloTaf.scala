package taf 
package snippet 

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import java.util.Date
import taf.lib._
import Helpers._
import java.text.SimpleDateFormat

class HelloTaf {
  lazy val date: Box[Date] = TafDependencyFactory.inject[Date] // inject the date

  val df = new SimpleDateFormat("dd.MM.yyyy HH:mm")
  //def render = "* *" #> date.map(_.toString)
  def render = "* *" #> date.map(df.format(_))

}

