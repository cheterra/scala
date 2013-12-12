package taf 
package snippet 

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import java.util.Date
import taf.lib._
import taf.model._
import Helpers._

import taf.db._
import net.liftweb._

import mapper._
import util._
import http._


class SelectedOffice {

    def list(html: NodeSeq) : NodeSeq = {
        toShow.flatMap(office =>
            bind("item", html,
                 "agency" -> office.agencynumber,
                 "bu"     -> office.businessunitno,
                 "chain"  -> office.travelagencychain,
                 "coop"   -> office.cooperation,
                 FuncAttrBindParam("agency_href", {html: NodeSeq =>
                   Text("orgCust?agency="+ (office.agencynumber))},"href"),
                 FuncAttrBindParam("bu_href", {html: NodeSeq =>
                   Text("orgCust?agency="+ (office.agencynumber) + "&bu="+ (office.businessunitno))},"href"),
                 FuncAttrBindParam("chain_href", {html: NodeSeq =>
                   Text("orgCust?chain="+ (office.travelagencychain))},"href"),
                 FuncAttrBindParam("coop_href", {html: NodeSeq =>
                   Text("orgCust?coop="+ (office.cooperation))},"href"),
                 FuncAttrBindParam("a_href", {html: NodeSeq =>
                   Text("orgCust?agency="+ (office.agencynumber))},"href"),
                 FuncAttrBindParam("b_href", {html: NodeSeq =>
                   Text("orgCust?agency="+ (office.agencynumber) + "&bu="+ (office.businessunitno))},"href"),
                 FuncAttrBindParam("c_href", {html: NodeSeq =>
                   Text("orgCust?chain="+ (office.travelagencychain))},"href"),
                 FuncAttrBindParam("d_href", {html: NodeSeq =>
                   Text("orgCust?coop="+ (office.cooperation))},"href"),
                 FuncAttrBindParam("select_href", {html: NodeSeq =>
                   Text("orgCust" 
                        + "?agency="+ (office.agencynumber)
                        + "&bu="    + (office.businessunitno))},"href")
            )
        )
    }

    private def toShow = {
      val buf = new StringBuilder()
      for (d <- ScreenSelectFeeTableSmall.all("office") ) buf.append(d)
      var sql = "select * from office where 1 = 2"
      if (buf.size > 0)
        sql = "select * from office where " + buf.substring(4)
      sql = sql.replace("agencyno", "agencynumber")
      println("sql: " + sql)
      val res = Office.findAllByInsecureSql(sql,
        IHaveValidatedThisSQL("frank", "2008-12-03")
      )
      ExportStore.register("office", new ListData(Office, res))
      res
    }

    def makeLinkExport(html: NodeSeq) : NodeSeq = {
      val buf = new StringBuilder()
      for (d <- ScreenSelectFeeTableSmall.all("office") ) buf.append(d)
      ExportUtil.doBind(html, "office", "", buf.substring(4))
    }

}
