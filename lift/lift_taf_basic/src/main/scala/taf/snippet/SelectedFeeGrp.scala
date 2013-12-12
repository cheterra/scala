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


class SelectedFeeGrp {
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
      val buf = new StringBuilder()
//      TafTOrgCust.findAll(By(TafTOrgCust.agencyNo,       S.param("agency") openOr ""), 
//                          By(TafTOrgCust.businessUnitNo, S.param("bu")     openOr "")    )
      // make a [GET param -> column name] map
      // feeGrpId, groupName, validFrom, validTo, state, orgId
      //"select * from Office where agency = xxx and bu = zzz"
      //if person "select * from Person where

      //def getAllOf(token: String): String = for (d <- ScreenSelectFeeTableSmall.all(token) ) buf.append(d)

      for (d <- ScreenSelectFeeTableSmall.all("agency") ) buf.append(d)
      println("agency: " + buf);
      val agency = buf.toString();
      val bu       = Box !! ScreenSelectFeeTableSmall.all("businessunit")(0)
      val customer = Box !! ScreenSelectFeeTableSmall.all("customer")(0)

      // select all from org_cust where agency, chain or coop is matching with agency record from table 'office'
      val sql = "select FEE_GRP_ID, AGENCY_NO, BUSINESS_UNIT_NO, CHAIN_ID, COOP_ID, CUST_NO, COMPANY_ID, CORPORATE_ID, CUST_TYPE" + " from taf_t_org_cust c, office o" + " where o.AGENCYNUMBER = '" + Office.checkAgency(agency) + "'" + """
 		 and
 		 (
   			o.AGENCYNUMBER = c.AGENCY_NO or
   			o.TRAVELAGENCYCHAIN = c.CHAIN_ID or
   			o.COOPERATION = c.COOP_ID
 		 )"""
      val agencies = TafTOrgCust.findAllByInsecureSql(sql, IHaveValidatedThisSQL("frank", "2008-12-03"))

      ExportStore.feeGrpList = customer match { 
        // "*" is like empty customer here
        case Full("*")   => findWithoutCust(agencies, bu)
        case Full(cust)  => findWithCust(cust, Office.checkAgency(agency), agencies, bu)
        case _           => findWithoutCust(agencies, bu)
      }
      //val orgCusts = 



      //sql = sql.replace("agencyno", "agencynumber")
      //println("sql: " + sql)
      //ExportStore.officeList = Office.findAllByInsecureSql(sql,
      //  IHaveValidatedThisSQL("frank", "2008-12-03")

      //val pList = Map ("agency" -> Office.agencyNo,
      //                 "bu"     -> Office.businessUnitNo,
      //                 "chain"  -> Person.customer)
      //TafTFeeGrp.findAll
      size = ExportStore.feeGrpList.length

      ExportStore.feeGrpList
    }

    def toFeeGrpList(orgcust: List[TafTOrgCust]) : List[TafTFeeGrp] = {
      def feeGrpIdList = for (o <- orgcust) yield o.feeGrpId

      val sqlWhere = if (feeGrpIdList.isEmpty || orgcust.isEmpty) " where 1 = 2 " 
                     else  ("where fee_grp_id in (" + Db.toSql(feeGrpIdList.distinct) + ")")
      println("sql: " + "select * from taf_t_fee_grp " + sqlWhere)
      TafTFeeGrp.findAllByInsecureSql("select * from taf_t_fee_grp " + sqlWhere,
        IHaveValidatedThisSQL("frank", "2008-12-03"))
    }

    def findWithoutCust(agencies: List[TafTOrgCust], bu: Box[String]) : List[TafTFeeGrp] = {
      println("without cust")
      val orgcust = findWithoutCustA(agencies, bu)
      toFeeGrpList(orgcust)
    }

    def findWithoutCustA(agencies: List[TafTOrgCust], bu: Box[String]) : List[TafTOrgCust] = {
      //println("without cust")
      //val agBu = orgCustByAgBu(agencies)
      //findWithCust("", agencies, bu)
      // 1) agency + business unit
      val agBu =  orgCustByAgBu(agencies)
      // 2) agency
      val ag =    orgCustByAg(agencies)
      // 3) chain
      val chain = orgCustByChain(agencies)
      // 4) coop
      val coop =  orgCustByCoop(agencies)
      //TafTFeeGrp.findAll
      //val orgcust = agBu openOr ag openOr Nil
      //val instanceList = List(agBu, ag, chain, coop)

      //println("bu as key: " + bu);
      // TEST coop with AG: 009496 (TCVERT), 000994 (ATL)
//      bu match {
//        case Full("ag")     => ag
//        case Full("agbu")   => agBu
//        case Full("chain")  => chain
//        case Full("coop")   => coop
//        // TODO xxx(0) not good solution
//        case _              => List(agBu, ag, chain, coop).filter(oc => oc.length > 0) (0)
//      }
      List(agBu, ag, chain, coop).filter(oc => oc.length > 0) (0)
    }


    // TODO exclude customer type 'A'
    def findWithCust(customer: String, agency: String, agencies: List[TafTOrgCust], bu: Box[String]): List[TafTFeeGrp] = {
      println("with cust: " + customer)
      var orgcust = findWithoutCustA(agencies, bu)
      // and now remove the customer (firmenkennzeichen, cooperationskennzeichen)
      println("org cust size: " + orgcust.length)

      // get additional customer attributes like company and coop
      val sql = "select * from person where agencyno = " + Office.checkAgency(agency) + " and customerno = " + customer
      // TODO xxx(0) not good solution
      var persons = Person.findAllByInsecureSql(sql, IHaveValidatedThisSQL("frank", "2008-12-03"))

      // person is empty? There is no fee table for that customer
      // TODO throw HTLM error
      if (persons == null || persons.isEmpty) {
        S.error("exception", "no-data-for-this-customer " + customer)
        // The ResponseShortcutException will make Lift shortcut (doh) and return
        // the file instead of continuing whatever it was doing to render the page.
        //throw ResponseShortcutException.shortcutResponse(response)
        return Nil
      }

      val person = persons (0)

      val cust = orgCustByCust(orgcust, person)
      val comp = orgCustByComp(orgcust, person)
      val corp = orgCustByCorp(orgcust, person)
      println("cust size: " + cust.length)
      println("comp size: " + comp.length)
      println("corp size: " + corp.length)
      orgcust = List(cust, comp, corp).filter(oc => oc.length > 0) head //(0)

      // TEST
      //orgcust.map(oc => println("org cust: " + oc))

      toFeeGrpList(orgcust)
    }

    def has(s: String) = String.valueOf(s).length > 0

    def orgCustByAgBu(agencies: List[TafTOrgCust]) = {
      for (oc <- agencies if has(oc.agencyNo) && has(oc.businessUnitNo)) yield oc
    }

    /** OrgCustByAG select all where agency is not null */
    def orgCustByAg(agencies: List[TafTOrgCust]) = {
      for (oc <- agencies if has(oc.agencyNo)) yield oc
    }

    /** OrgCustByChain select all where chain is not null */
    def orgCustByChain(agencies: List[TafTOrgCust]) = {
      for (oc <- agencies if has(oc.chainId)) yield oc
    }

    /** OrgCustByCoop select all where coop is not null */
    def orgCustByCoop(agencies: List[TafTOrgCust]) = {
      for (oc <- agencies if has(oc.coopId)) yield oc
    }

    // --- the three customer checks ---

    /** OrgCustByCust select all where customer is not null */
    def orgCustByCust(agencies: List[TafTOrgCust], person: Person) = {
      for (oc <- agencies if person.customerno.equals(oc.custNo)) yield oc
    }

    /** OrgCustByComp select all where company (Firmenkennzeichen) is not null */
    def orgCustByComp(agencies: List[TafTOrgCust], person: Person) = {
      agencies.filter(oc => person.companycode.equals(oc.companyId))
    }

    /** OrgCustByCorp select all where corporation (Konzernkennzeichen) is not null */
    def orgCustByCorp(agencies: List[TafTOrgCust], person: Person) = {
      agencies.filter(oc => person.corporatecode.equals(oc.corporateId))
    }

    def makeLinkExport(html: NodeSeq) : NodeSeq = {
      ExportUtil.doBind(html, "feegrp", "", "")
    }

}
