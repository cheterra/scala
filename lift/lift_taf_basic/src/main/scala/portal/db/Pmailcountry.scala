package portal.db

import common.db._

import scala.xml.{NodeSeq, Text}
import scala.collection.mutable.{ListBuffer}

import net.liftweb._

import mapper._
import util._
import http._
import common._
import db._

import scala.collection.mutable.{ListBuffer}


class Pmailcountry extends KeyedMapper[String, Pmailcountry] with ExportableList {
  def getSingleton = Pmailcountry

  def primaryKeyField = countrycode
  object countrycode        extends MappedStringIndex(this, 12)
  object description        extends MappedString(this, 38)
  object productlineid      extends MappedLongIndex(this)


  def getExportList = List(countrycode,productlineid,description)
}

object Pmailcountry extends Pmailcountry with KeyedMetaMapper[String, Pmailcountry] {
  override def dbDefaultConnectionIdentifier = portalDB

}



