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


class HelpdeskOffice {

    def list(html: NodeSeq) : NodeSeq = {
        toShow.flatMap(office =>
            bind("item", html,
                 "startnr" -> office.startnumber,
                 "agency"  -> office.agencynumber,
                 "bu"      -> office.businessunitno,
                 "chain"   -> office.travelagencychain,
                 "coop"    -> office.cooperation,
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
      var sql = """
select oid.ofi_totalid, 
off.agencynumber, off.businessunitno, off.travelagencychain, off.cooperation, off.accountingmode, off.name1,
cul.accounting_mode, cul.fee_grp_id, cul.agency_no, cul.business_unit_no, cul.chain_id, cul.coop_id, cul.cust_no, cul.company_id, cul.corporate_id
from officeid oid 
left join office off on startnumber = ofi_startnr
left join mv_taf_org_cust_list cul on (agencynumber = Agency_no or chain_id = off.travelagencychain or off.cooperation = cul.coop_id)
where ofi_totalid = 'FRALT2212'
order by cul.priority, cul.cust_no, cul.company_id, cul.corporate_id, cul.agency_no, cul.business_unit_no
"""
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


/*
Hi Frank,


hier ein Beispiel mit officeId:

select oid.ofi_totalid, 
off.agencynumber, off.businessunitno, off.travelagencychain, off.cooperation, off.accountingmode, off.name1,
cul.accounting_mode, cul.fee_grp_id, cul.agency_no, cul.business_unit_no, cul.chain_id, cul.coop_id, cul.cust_no, cul.company_id, cul.corporate_id
from officeid oid 
left join office off on startnumber = ofi_startnr
left join mv_taf_org_cust_list cul on (agencynumber = Agency_no or chain_id = off.travelagencychain or off.cooperation = cul.coop_id)
where ofi_totalid = 'FRALT2212'
order by cul.priority, cul.cust_no, cul.company_id, cul.corporate_id, cul.agency_no, cul.business_unit_no

Horst
*/