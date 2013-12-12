package common
package db

import net.liftweb._
import http._
import util._
import common._
import Helpers._

import scala.xml._
import http.js.jquery.JqJsCmds.DisplayMessage 
import http.js._

import net.liftweb.json.JsonAST
import net.liftweb.json.JsonDSL._
import net.liftweb.json.Printer._
import net.liftweb.json.Merge._
import net.liftweb.json.JsonParser
import net.liftweb.json.Extraction


/** Stores TableData on a page as base64 encoded JSON and retrieves the values from there */
class TableData {
  val log = Logger(getClass().getName())

  /** changed by URL param */
  var first = 0L
  /** changed by TableComponent.onPprChange */
  var rowsPerPage = 5
  var search = ""
  var lastSearch = ""
  var isNewSearch = false
  var col = ""
    
  /** init - get data from table page */
  def init = {
    S.param("tableData").map(s => {
      log.debug("init base64: " + s)
      val dataJson = decode(s)
      log.debug("init json: " + dataJson)
      val parsed = JsonParser.parse(dataJson)
      log.debug("init parsed: " + parsed)
      val mapped = Extraction.flatten(parsed)
      log.debug("init extracted: " + mapped)
      log.debug("init rpp: "    + mapped.getOrElse(".rpp", "rpp not found"))
      log.debug("init search: " + mapped.getOrElse(".search", "search not found"))
      search = mapped.getOrElse(".search", search)
      //first = mapped.getOrElse(".first", first).map(toLong) openOr first
      val strFirst = mapped.getOrElse(".first", first.toString)  // aaarrrgh
      first = strFirst.toLong
      col = mapped.getOrElse(".col", col)
    })
    // take the variable from the URL parameters if available
    search = S.param("search").map(String.valueOf(_)) openOr search
    search = removeQuotes(search)
    isNewSearch = !search.equals(lastSearch)
    lastSearch = search
    
    first =  S.param("first").map(toLong) openOr first
    // if this is a new search reset the index to 0
    adjustFirst
    col =    S.param("col").map(String.valueOf(_)) openOr col
    col = removeQuotes(col)
    log.debug("init finally: search: " + search + ", first: " + first + ", col: " + col)
  }
  
  /** store data on table page */
  def store (in: NodeSeq) = {
//      val json1 = ("rpp" -> rowsPerPage) ~ ("search" -> search) ~ ("first" -> first) ~ ("col" -> col)
    var json1 = ("rpp" -> rowsPerPage) ~ ("first" -> first)
//    if (!search.isEmpty)
//      json1 = json1 merge ("search" -> search)
//    if (!col.isEmpty)
//      json1 = json1 merge ("col" -> col)
    // TODO find a way to merge conditionally
    if (!search.isEmpty && !col.isEmpty)
      json1 = ("rpp" -> rowsPerPage) ~ ("first" -> first) ~ ("search" -> search) ~ ("col" -> col)
    else if (!search.isEmpty)
      json1 = ("rpp" -> rowsPerPage) ~ ("first" -> first) ~ ("search" -> search)
    else if (!col.isEmpty)
      json1 = ("rpp" -> rowsPerPage) ~ ("first" -> first) ~ ("col" -> col)
      
    //val json1 = Map("foo" -> 4, "bar" -> "baz")
    val dataJson = compact(JsonAST.render(json1))
    log.debug("store json: " + dataJson)
    val dataBase64 = encode(dataJson)
    log.debug("store b64: " + new String(dataBase64))
    <input type="hidden" name="tableData" value={dataBase64}/>
  }
  
  private def removeQuotes(text: String) =
    if (text.startsWith("\"") && text.endsWith("\"")) 
      text.substring(1, text.length - 1)
    else if (text.startsWith("\\\"") && text.endsWith("\\\"")) 
      text.substring(2, text.length - 2)
    else text
  
  protected def encode(text: String) =
    new sun.misc.BASE64Encoder().encode( text.getBytes())
  
  protected def decode (code: String): String =
    new String(new sun.misc.BASE64Decoder().decodeBuffer(code))

  protected def adjustFirst = if (isNewSearch) first = 0L

}
