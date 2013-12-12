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


class Pmail extends LongKeyedMapper[Pmail] with ExportableList {
  def getSingleton = Pmail

  def primaryKeyField = productlineid
  object productlineid  extends MappedLongIndex(this)
  object mailtype       extends PaddedChar(this, 1)
  object subject        extends MappedString(this, 100)
  // Strange, that field is always empty
  object body           extends MappedString(this, 2000)

  def getExportList = List(productlineid, subject)
}

object Pmail extends Pmail with LongKeyedMetaMapper[Pmail] {
  override def dbDefaultConnectionIdentifier = portalDB

}



