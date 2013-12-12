package portal
package snippet 

import scala.xml.{NodeSeq, Text}
import scala.collection.mutable.{ListBuffer}
import net.liftweb.util._

import net.liftweb.common._
import java.util.Date
import Helpers._

import portal.db._
import net.liftweb._

import mapper._
import util._
import http._


class SearchProduct {

    // TODO delete
    def list(html: NodeSeq) : NodeSeq = {
      html
    }

    // TODO delete
    private def toShow = {
      val sql = "select * from PPRODUCT order by PRODUCTID"
      Pproduct.findAllByInsecureSql(sql, IHaveValidatedThisSQL("frank", "2008-12-03"))
    }
    
    def getNextId: Long = {
      runCount + 1L
    }

    def runCount: Long = {
        //(query: String) : (List[String], List[List[String]])      
      val sql = "select max(PRODUCTID) from PPRODUCT"
      val res: (List[String], List[List[String]]) = DB.runQuery(sql, Nil, portalDB)
      // well, from the data (data = _2 = 2nd tuppel) get the record (0) and the column (0)
      res._2(0)(0).toLong
    }
    
    def create(id: Long) = {
      val sql = "insert into PPRODUCT (PRODUCTID, NAME, APPLICATION, productlineid, producttype, description)" +
        " values (?, ?, ?, ?, ?, ?)"
      val params = List(id, "name", "name", id, "U", "DESCRIPTION")
      println("sql: " + sql + " - " + params)
      DB.runUpdate(sql, params, portalDB)
      
      createStadiProd(id)
      createLicGroup(id)
      // the list must be updated now
      toShow;
    }
    
    def createStadiProd(id: Long) = {
      val sql = "INSERT INTO PSTADIPRODUCT (PRODUCTID, STADIID, DESCRIPTION, SHOWPLANNED) values (?, ?, ?, 'Y')"
      val params = List(id, "name", "desc")
      println("sql: " + sql + " - " + params)
      DB.runUpdate(sql, params, portalDB)
    }
      
    def createLicGroup(id: Long) = {
      val sql = "INSERT INTO plicensegroup(groupid,productid,licensetype) values ('V', ?, 'VEI')"
      val params = List(id)
      println("sql: " + sql + " - " + params)
      DB.runUpdate(sql, params, portalDB)
    }


    def showTable(in: NodeSeq): NodeSeq = {
      	// we will be here again when the button has been pressed
	    val newbut = S.param("new").map(String.valueOf(_)) openOr ""
	    println("new: " + newbut)
	    if (!newbut.isEmpty) {
	      // TODO find next id, create new record
	      val id = getNextId
	      println("next id: " + id)
	      create(id)
	      S.redirectTo(Pproduct.editPathString + "?val=" + id)
	    }
	    // the real work is done here
        Pproduct.showTable(in)
    }

    def editTable(in: NodeSeq): NodeSeq = {
      // TEST
      println("SearchProduct: edit " + in)
      Pproduct.editTable(in)
    }

}
