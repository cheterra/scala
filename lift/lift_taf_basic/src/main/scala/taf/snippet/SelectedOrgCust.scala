package taf 
package snippet 

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


class SelectedOrgCust {

    def list(html: NodeSeq) : NodeSeq = {
        toShow.flatMap(orgcust =>
            bind("item", html,
                 "agency" -> orgcust.agencyNo,
                 "bu"     -> orgcust.businessUnitNo,
                 "chain"  -> orgcust.chainId,
                 "coop"   -> orgcust.coopId,
                 "cust"   -> orgcust.custNo,
                 "comp"   -> orgcust.companyId,
                 "corp"   -> orgcust.corporateId,
                 "ctype"  -> orgcust.custType,
                 "feegrp" -> orgcust.feeGrpId,
                 //FuncAttrBindParam("select_href", {html: NodeSeq =>
                 //  Text("select/"+ (orgcust.primaryKeyField))},"href")
                 FuncAttrBindParam("select_href", {html: NodeSeq =>
                   Text("/taf_t_fee_grp/view/"+ (orgcust.feeGrpId))},"href")
            )
        )
    }

    private def toShow = {
      val buf = new StringBuilder()
      //var sql = "select * from taf_t_org_cust where agency_no = '009603'";
      println("agency: " + (S.param("agency") openOr " nix "))
      println("bu: "     + (S.param("bu")     openOr " nix "))
//      TafTOrgCust.findAll(By(TafTOrgCust.agencyNo,       S.param("agency") openOr ""), 
//                          By(TafTOrgCust.businessUnitNo, S.param("bu")     openOr "")    )
      // make a [GET param -> column name] map
      //val pList: Map[String, Mapper[MappedField[Any, Any]]] = Map (//"feegrpid"-> TafTOrgCust.feeGrpId,
      val pList = Map ("feegrpid"-> TafTOrgCust.feeGrpId,
                       "agency"  -> TafTOrgCust.agencyNo,
                       "bu"      -> TafTOrgCust.businessUnitNo,
                       "chain"   -> TafTOrgCust.chainId,
                       "coop"    -> TafTOrgCust.coopId,
                       "cust"    -> TafTOrgCust.custNo,
                       "comp"    -> TafTOrgCust.companyId,
                       "corp"    -> TafTOrgCust.corporateId )

      //val pListBy = new ListBuffer[Cmp[taf.db.TafTOrgCust,String]]

      // Seq[net.liftweb.mapper.QueryParam[taf.db.TafTOrgCust]
      val pListBy = new ListBuffer[QueryParam[TafTOrgCust]]
      // get the param from request and the column of from the map, make a where clause
      for ((key, field) <- pList) {
         // for type CHAR: By(field, "TUIG3 "))
         field match {
           //case f: MappedField[String, TafTOrgCust]  => S.param(key).foreach(x => pListBy += By(f, "xxx"))
           case f: PaddedString   [TafTOrgCust] => S.param(key).foreach(x => pListBy += By(f, f.pad(x)))
           case f: MappedLongIndex[TafTOrgCust] => S.param(key).foreach(x => pListBy += By(f, x.toLong))
         }
      }
      ExportStore.orgcustList = TafTOrgCust.findAll(pListBy :_*)
      ExportStore.orgcustList
    }

    def makeLinkExport(html: NodeSeq) : NodeSeq = {
      ExportUtil.doBind(html, "orgcust", "", "")
    }

}
