package taf.db

import _root_.java.math.MathContext
import _root_.net.liftweb.mapper._
import _root_.java.sql._
import net.liftweb.common._

//class Expense extends LongKeyedMapper[Expense] with IdPK {
//  def getSingleton = Expense
//  object dateOf extends MappedDateTime(this)
//  object description extends MappedString(this,100)
//  object amount extends MappedDecimal(this, MathContext.DECIMAL64, 2)
//  object account extends MappedLongForeignKey(this, Account)
//}

object DBVendor extends ConnectionManager {
  // Force load the driver
  Class.forName("org.postgresql.Driver")
  // define methods
  def newConnection(name : ConnectionIdentifier) = {
    try {
      Full(DriverManager.getConnection(
           "jdbc:postgresql://localhost/mydatabase",
           "root", "secret"))
    } catch {
      case e : Exception => e.printStackTrace; Empty
    }
  }
  def releaseConnection (conn : Connection) { conn.close }
}


case class TestThird(x: Int) {

  // test canceled. See ThirdExample
  def dodb : Unit =
  {
      println ("starting db transaction")
      DB.defineConnectionManager(DefaultConnectionIdentifier, DBVendor)
      println ("connection manager activated")
      try {

      } catch {
        case e: Exception => println ("Unexp. " + e)
      }
      println ("finished db transaction")
  }

}