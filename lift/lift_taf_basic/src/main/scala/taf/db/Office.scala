package taf.db

import scala.xml.{NodeSeq, Text}
import scala.collection.mutable.{ListBuffer}

import net.liftweb._

import mapper._
import util._
import http._
import common._

import scala.collection.mutable.{ListBuffer}


class Office extends LongKeyedMapper[Office] with ExportableList {
  def getSingleton = Office
  //override def dbDefaultConnectionIdentifier = tafDB

  //dbName = "Office"
  def primaryKeyField = startnumber
  object startnumber       extends MappedLongIndex(this)
  object agencynumber      extends PaddedString(this, 6)
  object businessunitno    extends PaddedString(this, 3)
  object travelagencychain extends MappedString(this, 6)
  object cooperation       extends MappedString(this, 6)

  def getExportList = List(agencynumber, businessunitno, travelagencychain, cooperation)

  val paramMap = Map ("agency" -> agencynumber,
                      "bu"     -> businessunitno)

}

object Office extends Office with LongKeyedMetaMapper[Office] {
  def findAllByParams = {
      val pListBy = new ListBuffer[Cmp[taf.db.Office,String]]
      // get the param from request and the column of from the map, make a where clause
      for ((key, field) <- paramMap) {
         // for type CHAR: By(field, "TUIG3 "))
         S.param(key).foreach(x => pListBy += By(field, field.pad(x)))
      }
      findAll(pListBy :_*)
  }

  def checkAgency(agency: String): String = {
    var res = agency.trim()
    if (res.length > 6) res = "000000"
    if (res.length < 6) res = "000000".substring(res.length) + res
    return res
  }
}



