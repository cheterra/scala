package portal.db

import common.db._

import scala.xml.{NodeSeq, Text}
import scala.collection.mutable.{ListBuffer}

import net.liftweb._

import mapper._
import util._
import http._
import common._

import scala.collection.mutable.{ListBuffer}


class Plicensegroup extends KeyedMapper[String, Plicensegroup] with ExportableList {
  def getSingleton = Plicensegroup

  def primaryKeyField = groupid
  object groupid          extends MappedStringIndex(this, 12)
  object productid        extends MappedLongIndex(this)
  object licensetype      extends MappedString(this, 12)

  def getExportList = List(groupid,productid,licensetype)
}

object Plicensegroup extends Plicensegroup with KeyedMetaMapper[String, Plicensegroup] {
  override def dbDefaultConnectionIdentifier = portalDB

}



