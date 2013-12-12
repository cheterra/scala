package france
package model

import scala.xml._
import net.liftweb.util._
import net.liftweb.common._
import java.util.Date



class Card (card: Node) {
  // performance issue
  var df = new java.text.SimpleDateFormat("dd-MMM-yyyy HH:mm:ss");
// <Card TestsHit="3" TestsTotal="3" DateExpired="01-Nov-2006 12:05:21" DateTested="02-Oct-2006 13:05:21" DateTouched="02-Oct-2006 13:06:41" DateCreated="08-Sep-2006 10:56:23" Backside="Danke" Frontside="xxx"></Card>
//  def text(name: String) = (card \ name).text

//  def attr(name: String, default: String): String = 
//    text(name) match {
//      case x => if (x.isEmpty) default else x
//    }

  def iattr(name: String, default: Int):  Int  = attr(name, default, {Integer.parseInt(_)})
  def dattr(name: String, default: Date): Date = attr(name, default, {df.parse(_)})

  /** This is a converter function which searches for an attribute n,
   *  @param name: the name of the attribute
   *  @param default: the default value if the attribute could not be found
   *  @param conv: a function which converts the string found into the desired type
   *  @return the found and converted attribute or the default
   */
  def attr[A](name: String, default: A, conv: String => A ): A = {
    val x = (card \ name).text
    if (x.isEmpty) default else conv(x)
  }

  lazy val sideA = (card \ "@Frontside").text
  lazy val sideB = (card \ "@Backside").text
  lazy val hits  = iattr("@TestsHit",   0)
  lazy val total = iattr("@TestsTotal", 0)
  lazy val dateCreated = df.parse((card \ "@DateCreated").text)
  lazy val dateExpired = dattr("@DateExpired", null)
  lazy val dateTested  = dattr("@DateTested",  null)
  lazy val dateTouched = dattr("@DateTouched", new Date)

  //def reset = { hits = 0; total = 0 }

  def toParam =  "Card: [sideA: " + sideA + ", sideB: " + sideB + "\n" +
                       ", hits: " + hits + ", total: " + total + "\n" +
                       ", created: " + dateCreated + ", expired: " + dateExpired + "\n" +
                       ", touched: " + dateTouched + ", tested: "  + dateTested + "\n" +
                       "] \n card: \n" + card

  override def toString =  "Card: [sideA: " + sideA +
                           ", hits: " + hits + ", total: " + total +
                           ", expired: " + dateExpired +
                           ", created: " + dateCreated +
                           "] ------------ card: " + card

}
