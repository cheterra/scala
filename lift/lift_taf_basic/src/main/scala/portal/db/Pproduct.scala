package portal.db

import common.db._

import scala.xml.{NodeSeq, Text}
import scala.collection.mutable.{ListBuffer}

import net.liftweb._

import mapper._
import util._
import http._

import scala.collection.mutable.{ListBuffer}


class Pproduct extends LongKeyedMapper[Pproduct]  with ExportableList {
  def getSingleton = Pproduct

  def primaryKeyField = productid
  object productid          extends MappedLongIndex(this)  with ListView
  object name               extends MappedString(this, 32) with ListView
  object stadiid            extends MappedString(this, 10)
  object application        extends MappedString(this, 10)
  object description        extends MappedString(this, 80) with ListView
  object producttype        extends PaddedChar(this, 1)
  object productlineid      extends MappedLong(this)
  object lastchanged        extends MappedDate(this)
  object sendemail          extends MappedString(this, 20)
  object directloginpath    extends MappedString(this, 120)
  object mailmode           extends MappedString(this, 4)
  object articlemail        extends MappedString(this, 80)
  //lastchanged,producttype,sendemail,directloginpath,productlineid,mailmode,articlemail

  
  def getExportList = List(productid, name, description)
}

object Pproduct extends Pproduct with LongKeyedMetaMapper[Pproduct] with TABLEify[Long,Pproduct]{
  override def dbDefaultConnectionIdentifier = portalDB

  override def defaultSortingCol = "name"
    
  override lazy val editPathString: String = mp(editPath) + "/screenSelectedProduct"   
  
  override def listviewer: List[FieldPointerType] = {
    productid      wi = 50
    productid      selectable = true
    
    description    wi = 500;  
    description    sortable = false;
    
    List(productid, name, description)
  }
  
  
  /** display only the fields in 'listviewer' */
  override def fieldsForList = listviewer //List(productid, name, description)
  
}
