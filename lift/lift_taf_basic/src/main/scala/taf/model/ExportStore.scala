package taf 
package model

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import java.util.Date
import taf.lib._
import Helpers._

import taf.db._
import net.liftweb._

import mapper._
import util._
import http._


class TextTable(var head: List[String], var body: List[List[String]]) {
}

//class Model {
  // better way
//  var allLists = scala.collection.mutable.Map[String, ListData]()
//}

/** This is meant to hold exportable data */
object ExportStore {
  // TODO use session object theStore (Google: lift session variable)
  //object theStore extends SessionVar[Box[Model]]("Default")  // (Empty)

//  var officeList:  List[taf.db.Office]       = Nil
  var personList:  List[taf.db.Person]       = Nil
  var orgcustList: List[taf.db.TafTOrgCust]  = Nil
  var feeGrpList:  List[taf.db.TafTFeeGrp]   = Nil

  // better way
  var allLists = scala.collection.mutable.Map[String, ListData]()
  object allListsVar extends RequestVar(scala.collection.mutable.Map[String, ListData]())

  def register (name: String, data: ListData) = { allLists.put(name, data) }
  //def register (name: String, data: ListData) = { theStore.is.map(allLists.put(name, data)) }


  def makeTextTable(header: ExportableList, data: List[ExportableList]): TextTable = {
    // dbColumnName = org_cust_id, orgcust.name = orgCustId
    val head = header.getExportList.map(_.name)
    val body = data.map(row => row.getExportList.map("" + _))
    new TextTable(head, body)
  }

//  def makeOfficeText  = makeTextTable(Office,      officeList)
  def makePersonText  = makeTextTable(Person,      personList)
  def makeOrgCustText = makeTextTable(TafTOrgCust, orgcustList)
  def makeFeeGrpText  = makeTextTable(TafTFeeGrp,  feeGrpList)
  // better way
  /** Make a TextTable from a registered list
   * @param token Example: 'feegrp'
   */
  def makeTextTableReg(token: String) = {
    allLists.get(token) match {
      case None       => throw new IllegalArgumentException("token " + token + " is not in the list. We have only: " + allLists)
      case Some(data) => makeTextTable(data.header, data.datalist)
    }
  }

  def makeTextList(token: Box[String]): String = new TextListCreator().makeTheList(token)
  def makeCsvList (token: Box[String]): String = new CsvListCreator() .makeTheList(token)
}

class ListData (var header: ExportableList, var datalist: List[ExportableList]) {
}

/** ListCreator - creates text list or csv list */
abstract class ListCreator {

  val constBlank = "                                                                                            "
  val constDash  = "--------------------------------------------------------------------------------------------"
  def fill(filler: String, n: Int): String = filler.substring(0, math.min(filler.length - 1, n))
  def rightPad(txt: String, n: Int): String = txt + fill(constBlank, n - txt.length) // = substring(txt.length, n)
  def leftPad(txt: String, n: Int): String = String.format("%" + n + "s", txt)
  def underline(n: Int): String = fill(constDash, n)

  def makeTheList(token: Box[String]): String = {
      println("make the list for: " + token)
      // scala: this pattern matching should be replaced by an executable object in a map
      // TODO: get rid of the trailing space
      val s = (token openOr "none").trim
      s match { 
//        case "office"  => makeList(ExportStore.makeOfficeText)
        case "person"  => makeList(ExportStore.makePersonText)
        case "orgcust" => makeList(ExportStore.makeOrgCustText)
        case "feegrp"  => makeList(ExportStore.makeFeeGrpText)
        // better way
        case "org"     => makeList(ExportStore.makeTextTableReg(s))
        case "pay"     => makeList(ExportStore.makeTextTableReg(s))
        case "office"  => makeList(ExportStore.makeTextTableReg(s))
        case "zombies" => makeList(ExportStore.makeTextTableReg(s))
        case _ => throw new IllegalArgumentException("Export failed. Don't know what to export. token: " + token + "?"
                           + " There is a list of tokens I can process in ExportStore.scala"
                           + " and probably you have to add your token"
                           ); 
        //return "Export failed. Don't know what to export. What should I do with token: " + token + "?"
      }
      // This did not work probably due to a trailing space in 'office '
//      token match { 
//        case Full("office") => makeTextList(makeOfficeText)
//        case Full("person") => makeTextList(makePersonText)
//        case _ => println("Export failed. Don't know what to export."); return "Export failed. Don't know what to export."
//      }
  }

  def makeList(tt:TextTable): String
}

/** Specifig ListCreator for Text */
class TextListCreator extends ListCreator {
  override def makeList(tt:TextTable): String = {
    println("makeTextList -->")
    val buf = new StringBuilder()
    // measure
    val maxCol = new Array[Int](tt.head.size);

    // add a space of 3 chars behind the header and one behind the value
    for (i <- 0 until tt.head.length) maxCol(i) = math.max(maxCol(i), tt.head(i).length + 3)
    for (d <- tt.body ) for (i <- 0 until tt.head.length) maxCol(i) = math.max(maxCol(i), d(i).length + 1)

    // head
    for (i <- 0 until tt.head.length) buf.append(rightPad(tt.head(i), maxCol(i)))
    buf.append("\n")
    // underline
    for (i <- 0 until tt.head.length) buf.append(underline(maxCol(i) - 2)).append("  ")
    buf.append("\n")
    for (d <- tt.body ) {
      for(i <- 0 until tt.head.length) (buf.append(rightPad(d(i), maxCol(i))))
      buf.append("\n")
    }

    println("table:\n" + buf)
    println("makeTextList <--")
    buf.toString()
  }
}

class CsvListCreator extends ListCreator {
  override def makeList(tt:TextTable): String = {
    println("makeCsvList -->")
    val sep = ";"
    val buf = new StringBuilder()
    def last = buf.length() - 1
    def replaceLast(s: String) = if (last > 0) buf.replace(last, last + 1, s)
    def toLine(line: List[String]) = { line.flatMap(col => buf.append(col).append(sep)); replaceLast("\n") }
    toLine(tt.head)
    for (d <- tt.body ) toLine(d)
    println("table:\n" + buf)
    println("makeCsvList <--")
    buf.toString()
  }
}