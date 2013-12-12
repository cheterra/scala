package taf
package snippet
//package zombies

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


class SelectedZombies {
    var size: Int = 0

    def list(html: NodeSeq) : NodeSeq = {
        toShow.flatMap(feegrp =>
            bind("item", html,
                 FuncAttrBindParam("feegrpid_href", {html: NodeSeq =>
                   Text("orgCust?feegrpid="+ (feegrp.feeGrpId))},"href"),
                 "feegrpid"   -> feegrp.feeGrpId,
                 "groupname"  -> feegrp.groupName,
                 "validfrom"  -> feegrp.validFrom,
                 "validto"    -> feegrp.validTo,
                 "state"      -> feegrp.state,
                 "orgid"      -> feegrp.orgId,
                 "servicecode"-> feegrp.serviceCode,
                 "variantid"  -> feegrp.variantId,
                 FuncAttrBindParam("select_href", {html: NodeSeq =>
                   Text("/taf_t_fee_grp/view/"+ (feegrp.feeGrpId))},"href")
                 // http://localhost:8880/taf/scope/orgCust?cust=111
            )
        )
    }

    def footer(html: NodeSeq) : NodeSeq = {
      bind("crud", html,
         "count"    -> size,
         "first"    -> "nix"
//         "last"   -> crudAllLast() _, 
      )
    }

    private def toShow = {
      // find zombies, (flight) variants in fee models which do not exist anymore
      val sql = """
        -- select fee table and fee group
	select * from taf_t_fee f, taf_t_fee_grp g where substr(VARIANT_ID,0,2) not in 
	(
          -- which has stale variants (VARIANT_ID not in VARIANT_VGV anymore)
	  select distinct substr(VARIANT_ID,0,2) from TAF_T_SERVICE_CODE_VARIANT_VGV
          -- the variant may still exist as rail variant. Check only flight variants.
	  where SERVICE_CODE in
	  (
	    select SERVICE_CODE from taf_t_service_code_desc where category = 'F'
	  )
	)
        -- we have surely found some rail variants. Ignore everything but flight variants.
        and SERVICE_CODE in
	(
	  select distinct SERVICE_CODE from taf_t_service_code_desc where category = 'F'
	)
	and f.FEE_GRP_ID = g.FEE_GRP_ID
        -- speed up
        and variant_id is not null
        -- ignore old tables
	and g.VALID_TO > sysdate
        -- hint: variant id without airline code: substr(VARIANT_ID,0,2)
		 """
      val res = Zombies.findAllByInsecureSql(sql, IHaveValidatedThisSQL("frank", "2008-12-03"))
      size = res.length
      ExportStore.register("zombies", new ListData(Zombies, res))
      res
    }

    def makeLinkExport(html: NodeSeq) : NodeSeq = {
      ExportUtil.doBind(html, "zombies", "", "")
    }

}

/*
        -- select fee table and fee group
	select * from taf_t_fee f, taf_t_fee_grp g where substr(VARIANT_ID,0,2) not in 
	(
          -- which has stale variants (VARIANT_ID not in VARIANT_VGV anymore)
	  select distinct substr(VARIANT_ID,0,2) from TAF_T_SERVICE_CODE_VARIANT_VGV
          -- the variant may still exist as rail variant. Ignore rail variants
          -- better: 'where service code is fligth'
	  where SERVICE_CODE not in
	  (
	    select SERVICE_CODE from taf_t_service_code_desc where category = 'R'
	  )
	)
        -- remove that (was a 'just to be sure')
	and SERVICE_CODE in ('9948', '9945', '9944', '9406', '9407', '9408')
        -- TODO and add 'where service code is flight'
	and f.FEE_GRP_ID = g.FEE_GRP_ID
	and g.VALID_TO > sysdate
        -- hint: variant id without airline code: substr(VARIANT_ID,0,2)

*/
