package taf
package snippet
//package payment

import scala.xml._
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


class SelectedPaymentOrga {
    var size: Int = 0
    def count = size
    def theClass = "SelectedPaymentOrga"

    def list(html: NodeSeq) : NodeSeq = {
        toShow.flatMap(org =>
            bind("item", html,
                 "orgid"   -> org.orgId,
                 "orgname"  -> org.orgName,
                 FuncAttrBindParam("select_href", {html: NodeSeq =>
                   Text("/taf_t_admin_org/view/"+ (org.orgId))},"href")
            )
        )
    }

    def footer(html: NodeSeq) : NodeSeq = {
      bind("crud", html, "count" -> size)
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
select ORG_ID, ORG_NAME from taf_t_admin_org where org_id in
(
  select distinct org_id from taf_t_fee_grp where fee_grp_id in
  (
    select distinct FEE_GRP_ID from taf_t_surcharge where SURCHARGE <> '         000'
  )
)
 		 """
      val res = Orga.findAllByInsecureSql(sql, IHaveValidatedThisSQL("frank", "2008-12-03"))
      size = res.length
      ExportStore.register("org", new ListData(Orga, res))
      res
    }

    def makeLinkExport(html: NodeSeq) : NodeSeq = {
      ExportUtil.doBind(html, "org", "", "")
    }

    def generate(html: NodeSeq) : NodeSeq = {
      bind("crud", html, "template"    -> showAllTemplate)
    }

/*
   * Customize the display of a row for displayRecord
  protected def doDisplayRecordRow(entry: TheCrudType)(in: NodeSeq): NodeSeq =
    for {
      pointer <- fieldsForDisplay
      field <- computeFieldFromPointer(entry, pointer).toList
      if field.shouldDisplay_?
      node <- bind("crud", in, 
                   "name" -> field.displayHtml, 
                   "value" -> field.asHtml)
    } yield node

*/

    var columns = List("orgId", "orgName")

    def mkHead = {
      //  <th><span class="lift:Loc.orgId"  >OrgId</span></th>
      //  <th><span class="lift:Loc.orgName">OrgName</span></th>
      columns.map(col => XML.loadString(
        "<th><span class=lift.Loc" + col + ">" + col.capitalize + "</span></th>"))
    }

    def theHead = 
        <tr>
            <th><span class="lift:Loc.orgId">OrgId</span></th>
            <th><span class="lift:Loc.orgName">OrgName</span></th>
        </tr>

    def theBody =
        <tr>
            <td><item:orgid/></td>
            <td><item:orgname/></td>
        </tr>


    val bodySnippet   = "lift:" + theClass + ".list"
    val footerSnippet = "lift:" + theClass + ".footer"
    val exportSnippet = "lift:" + theClass + ".makeLinkExport"

    def showAllTemplate =
<div>
  <table>
      <thead>
            {theHead}
      </thead>
      <tbody class={bodySnippet}>
                  {theBody}
      </tbody>
      <tfoot class={footerSnippet}>
         <tr>
           <td colspan="6">
              <span class="lift:Loc.rows">Rows</span>:&nbsp;&nbsp;<crud:count>{count}</crud:count>
           </td>
         </tr>
      </tfoot>
  </table>
  <span class={bodySnippet}>
	<div>Export as <a exp:clip_href="">text</a> to clipboard or <a exp:file_href="">csv</a> to file</div>
  </span>
  <span class="hint"> (generated by {theClass}) </span>
</div>

}


/*
    def theHead = 
        <tr>
            <th><span class="lift:Loc.orgId">OrgId</span></th>
            <th><span class="lift:Loc.orgName">OrgName</span></th>
        </tr>

    def theBody =
        <tr>
            <td><item:orgid/></td>
            <td><item:orgname/></td>
        </tr>

    def showAllTemplate =
<div>
  <table>
      <thead>
            {theHead}
      </thead>
      <tbody class="lift:SelectedPaymentOrga.list">
                  {theBody}
      </tbody>
      <tfoot class="lift:SelectedPaymentOrga.footer">  
         <tr>  
           <td colspan="6">
              <span class="lift:Loc.rows">Rows</span>:&nbsp;&nbsp;<crud:count>{count}</crud:count>
           </td>  
         </tr>  
      </tfoot>  
  </table>
  <span class="lift:SelectedPaymentOrga.makeLinkExport">
	<div>Export as <a exp:clip_href="">text</a> to clipboard or <a exp:file_href="">csv</a> to file</div>
  </span>
  <span class="hint"> (generated by {theClass}) </span>
</div>

*/