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


class SelectedPerson {

    def list(html: NodeSeq) : NodeSeq = {
        toShow.flatMap(pers =>
            bind("item", html,
                 "agency" -> pers.agencyno,
                 "bu"     -> pers.businessunitno,
                 "cust"   -> pers.customerno,
                 "comp"   -> pers.companycode,
                 "corp"   -> pers.corporatecode,
                 FuncAttrBindParam("ag_href", {html: NodeSeq =>
                   Text("orgCust?agency="+ (pers.agencyno) + "&cust="+ (pers.customerno))},"href"),
                 FuncAttrBindParam("bu_href", {html: NodeSeq =>
                   Text("orgCust?agency="+ (pers.agencyno) + "&bu="+ (pers.businessunitno) + "&cust="+ (pers.customerno))},"href"),
                 FuncAttrBindParam("cp_href", {html: NodeSeq =>
                   Text("orgCust?comp="+ (pers.companycode)   + "&agency="+ (pers.agencyno))},"href"),
                 FuncAttrBindParam("cr_href", {html: NodeSeq =>
                   Text("orgCust?corp="+ (pers.corporatecode) + "&agency="+ (pers.agencyno))},"href"),
                 FuncAttrBindParam("agency_href", {html: NodeSeq =>
                   Text("orgCust?agency="+ (pers.agencyno))},"href"),
                 FuncAttrBindParam("cust_href", {html: NodeSeq =>
                   Text("orgCust?cust="+ (pers.customerno))},"href"),
                 FuncAttrBindParam("comp_href", {html: NodeSeq =>
                   Text("orgCust?comp="+ (pers.companycode))},"href"),
                 FuncAttrBindParam("corp_href", {html: NodeSeq =>
                   Text("orgCust?corp="+ (pers.corporatecode))},"href")
            )
        )
    }

    private def toShow = {
      val buf = new StringBuilder()
      for (d <- ScreenSelectFeeTableSmall.all("person") ) buf.append(d)
      println("where clause: " + buf)
      ExportStore.personList = Person.findAllByInsecureSql("select * from person where " + buf.substring(4),
        IHaveValidatedThisSQL("frank", "2008-12-03")
      )
      ExportStore.personList
    }

    def makeLinkExport(html: NodeSeq) : NodeSeq = {
      val buf = new StringBuilder()
      for (d <- ScreenSelectFeeTableSmall.all("person") ) buf.append(d)
      ExportUtil.doBind(html, "person", "", buf.substring(4))
    }
}
