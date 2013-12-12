package taf
package snippet
//package payment

import scala.xml.{NodeSeq, Text}
import scala.collection.mutable.{ListBuffer}
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


class SelectedPaymentFee {
    var size: Int = 0

    def list(html: NodeSeq) : NodeSeq = {
        toShow.flatMap(grp =>
            bind("item", html,
                 "orgname"  -> grp.orgName,
                 FuncAttrBindParam("feegrpid_href", {html: NodeSeq =>
                   Text("orgCust?feegrpid="+ (grp.feeGrpId))},"href"),
                 "groupname"  -> grp.groupName,
                 "state"      -> grp.state,
                 "surcharge"  -> grp.surcharge,
                 "vatid"      -> grp.vatId,
                 "salesdocumenttype"  -> grp.salesDocumentType,
                 "formofpayment"      -> grp.formOfPayment
            )
        )
    }

    def footer(html: NodeSeq) : NodeSeq = {
      bind("crud", html,
         "count"    -> size,
         "first"    -> "nix"
      )
    }

/*
- For Tansu a tool to get the payment fee statistic, mit dem man die Payment-Fee-Statistik abfragen kann:
(two tables)
select ORG_ID, ORG_NAME from taf_t_admin_org where org_id in
(
  select distinct org_id from taf_t_fee_grp where fee_grp_id in
  (
    select distinct FEE_GRP_ID from taf_t_surcharge where SURCHARGE <> '         000'
  )
)
.
And now detailed but without BCD
.
select org_name, GROUP_NAME, STATE, SURCHARGE, VAT_ID, SALES_DOCUMENT_TYPE, FORM_OF_PAYMENT from taf_t_admin_org o, taf_t_fee_grp g, taf_t_surcharge s where
o.org_id = g.org_id and
(
  o.org_id = s.org_id
  or
  g.fee_grp_id = s.fee_grp_id
)
and  s.SURCHARGE <> '         000'
and o.org_id <> 170
and g.valid_to > TO_DATE('02.03.2012', 'DD.MM.YYYY')
.
*/
    private def toShow = {
      val sql = """
select org_name, GROUP_NAME, STATE, SURCHARGE, VAT_ID, SALES_DOCUMENT_TYPE, FORM_OF_PAYMENT from taf_t_admin_org o, taf_t_fee_grp g, taf_t_surcharge s where
o.org_id = g.org_id and
(
  o.org_id = s.org_id
  or
  g.fee_grp_id = s.fee_grp_id
)
and  s.SURCHARGE <> '         000'
and o.org_id <> 170
and g.valid_to > sysdate
 		 """
      val res = PayGrp.findAllByInsecureSql(sql, IHaveValidatedThisSQL("frank", "2008-12-03"))
      size = res.length
      ExportStore.register("pay", new ListData(PayGrp, res))
      res
    }

    def makeLinkExport(html: NodeSeq) : NodeSeq = {
      ExportUtil.doBind(html, "pay", "", "")
    }

}
