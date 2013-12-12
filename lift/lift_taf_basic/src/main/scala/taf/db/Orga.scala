package taf.db

import scala.xml.{NodeSeq, Text}
import scala.collection.mutable.{ListBuffer}

import net.liftweb._

import mapper._
import util._
import http._
import common._

import scala.collection.mutable.{ListBuffer}


class Orga extends LongKeyedMapper[Orga] with ExportableList {
  def getSingleton = Orga
  
  //dbName = "TAF_T_ADMIN_ORG"
  def primaryKeyField = orgId
  object orgId     extends MappedLongIndex(this)
  object orgName   extends MappedString(this, 6)

  def getExportList = List(orgId, orgName)

}

object Orga extends Orga with LongKeyedMetaMapper[Orga] {
}



