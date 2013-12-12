package portal.db

import common.db._

import scala.xml.{NodeSeq, Text}
import scala.collection.mutable.{ListBuffer}

import net.liftweb._

import mapper._
import util._
import http._


import scala.collection.mutable.{ListBuffer}


class Pstadiproduct extends LongKeyedMapper[Pstadiproduct] with ExportableList {
  def getSingleton = Pstadiproduct

  def primaryKeyField = productid
  object productid          extends MappedLongIndex(this)
  object stadiid            extends MappedString(this, 12)
  object description        extends MappedString(this, 12)
  object showplanned        extends PaddedChar(this, 1)

  def getExportList = List(productid,stadiid,description,showplanned)
}

object Pstadiproduct extends Pstadiproduct with LongKeyedMetaMapper[Pstadiproduct] {
  override def dbDefaultConnectionIdentifier = portalDB

}



