package common
package model

import common.db.ExportableList
import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import java.util.Date
import Helpers._

import net.liftweb._

import mapper._
import util._
import http._

class ListData (var header: ExportableList, var datalist: List[ExportableList]) {
}

/** Store is kept in the session */
object StoreVarSes extends SessionVar[ExportStore](new ExportStore) {
  // not really required. You could also use
  //   var store = StoreVar.is
  // instead of
  //   var store = StoreVar.getStore
  def getStore = StoreVarSes.is
}

/** Store per request (was RequestVar)*/
// TODO that should be RequestVar but I didn't get it working
object StoreVarReq extends SessionVar[ExportStore](new ExportStore) {
  val log = Logger("common.model.StoreVarReq")

  // not really required. You could also use
  //   var store = StoreVar.is
  // instead of
  //   var store = StoreVar.getStore
  def getStore = StoreVarReq.is
  def register (name: String, table: ExportableList, datalist: List[ExportableList]) = 
  {
    log.debug("registered: " + name)
    getStore.register(name, new ListData(table, datalist))
  }
}

class TextTable(var head: List[String], var body: List[List[String]]) {
}

/** This is meant to hold exportable data */
class ExportStore {
  val log = Logger(getClass().getName())

  var allLists = scala.collection.mutable.Map[String, ListData]()
  //object allListsVar extends RequestVar(scala.collection.mutable.Map[String, ListData]())

  // ExportStore.register("office", new ListData(Office, (Office.findAllBy(...))))
  def register (name: String, data: ListData) = { allLists.put(name, data) }
  //def register (name: String, data: ListData) = { theStore.is.map(allLists.put(name, data)) }


  def makeTextList(token: Box[String]): String = new TextListCreator().makeTheList(token)
  def makeCsvList (token: Box[String]): String = new CsvListCreator() .makeTheList(token)
  
  
	/** ListCreator - creates text list or csv list */
	abstract class ListCreator() {
	
	  val constBlank = "                                                                                            "
	  val constDash  = "--------------------------------------------------------------------------------------------"
	  def fill(filler: String, n: Int): String = filler.substring(0, math.min(filler.length - 1, n))
	  def rightPad(txt: String, n: Int): String = txt + fill(constBlank, n - txt.length) // = substring(txt.length, n)
	  def leftPad(txt: String, n: Int): String = String.format("%" + n + "s", txt)
	  def underline(n: Int): String = fill(constDash, n)
	
	  def makeTextTable(header: ExportableList, data: List[ExportableList]): TextTable = {
	    // dbColumnName = org_cust_id, orgcust.name = orgCustId
	    val head = header.getExportList.map(_.name)
	    val body = data.map(row => row.getExportList.map("" + _))
	    new TextTable(head, body)
	  }
	
	  /** Make a TextTable from a registered list
	   * @param token Example: 'feegrp'
	   */
	  def makeTextTableReg(token: String) = {
	    allLists.get(token) match {
	      case None       => throw new IllegalArgumentException(
	          "Token " + token + " is not in the list. We have only: " + allLists + 
	          "   Use 'register' to add the data to the store")
	      case Some(data) => makeTextTable(data.header, data.datalist)
	    }
	  }
	
	  def makeTheList(token: Box[String]): String = {
	      log.debug("make the list for: " + token)
	      // TODO: get rid of the trailing space
	      makeList(makeTextTableReg((token openOr "none").trim))
	  }
	
	  def makeList(tt:TextTable): String
	}
	
	/** Specifig ListCreator for Text */
	class TextListCreator() extends ListCreator {
	  override def makeList(tt:TextTable): String = {
	    log.debug("makeTextList -->")
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
	
	    log.debug("table:\n" + buf)
	    log.debug("makeTextList <--")
	    buf.toString()
	  }
	}
	
	class CsvListCreator() extends ListCreator {
	  override def makeList(tt:TextTable): String = {
	    log.debug("makeCsvList -->")
	    val sep = ";"
	    val buf = new StringBuilder()
	    def last = buf.length() - 1
	    def replaceLast(s: String) = if (last > 0) buf.replace(last, last + 1, s)
	    def toLine(line: List[String]) = { line.flatMap(col => buf.append(col).append(sep)); replaceLast("\n") }
	    toLine(tt.head)
	    for (d <- tt.body ) toLine(d)
	    log.debug("table:\n" + buf)
	    log.debug("makeCsvList <--")
	    buf.toString()
	  }
	}  
  
}


