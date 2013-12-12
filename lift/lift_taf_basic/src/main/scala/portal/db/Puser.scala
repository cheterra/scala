package portal.db

import common.db._

import scala.xml.{NodeSeq, Text}
import scala.collection.mutable.{ListBuffer}

import net.liftweb._

import mapper._
import util._
import http._

import scala.collection.mutable.{ListBuffer}


class Puser extends LongKeyedMapper[Puser] with ExportableList {
  def getSingleton = Puser

  //dbName = "PUSER"
  def primaryKeyField = pid
  object pid          extends MappedLongIndex(this)
  object name         extends MappedString(this, 38)

  def getExportList = List(pid, name)
}

object Puser extends Puser with LongKeyedMetaMapper[Puser] {
  override def dbDefaultConnectionIdentifier = portalDB

}



