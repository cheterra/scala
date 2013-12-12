package taf
package snippet

import net.liftweb._
import http._
import mapper._
import taf.db._

/**
 * Declare the fields on the screen
 */
object ScreenSelectFeeTable extends LiftScreen {
  // this is also a StatefullSnippet. Surprise!
  // here are the fields and default values
  val ag =    myField("Agency",         TafTOrgCust.agencyNo.dbColumnName)
  val bu =    myField("Business Unit",  TafTOrgCust.businessUnitNo.dbColumnName)
  val chain = myField("Chain",          TafTOrgCust.chainId.dbColumnName)
  val coop =  myField("Coop",           TafTOrgCust.coopId.dbColumnName)
  val cust =  myField("Customer",       TafTOrgCust.custNo.dbColumnName)
  val comp =  myField("Company",        TafTOrgCust.companyId.dbColumnName)
  val corp =  myField("Corperation",    TafTOrgCust.corporateId.dbColumnName)
  //val acc =   myField("Accounting Mode", TafTOrgCust.chainId.name)
  //link("/editAcct", () => currentAccountVar(acct), Text("Edit"))
  val xyz = field("Agency", "")

//  var dList = List(ag, bu, chain, coop, cust, comp, corp, acc)
  var dList = List(ag, bu, chain, coop, cust, comp, corp)

  //def dbFieldName (col: MappedField[FieldType, OwnerType]): String = col.name

  def myField(name: String, acc: String) = new TafFeeGroupField(field(S.?(name), ""), acc)

  object allData extends RequestVar(List(ag.is))

  def finish() {
    //S.notice("Agency: "+ag)
    //S.notice("Business Unit: "+bu)

    allData(dList.map(_.is))

    println("#### we are ScreenSelectFeeTable ####")
    println("get allData: " + ScreenSelectFeeTable.allData)
    println("#### leaving ScreenSelectFeeTable ####")

    // Pass the state around in a RequestVar by setting injector functions in your page transition functions (e.g. SHtml.link, S.redirectTo, etc). We cover this technique in Section 3.11. 

    // If you are using a StatefulSnippet (Section 5.3.3), use this.redirectTo so that your snippet instance is used when the redirect is processed. (from http://exploring.liftweb.net/master/index-3.html#toc-Section-3.9)
    //S.redirectTo("/taf_t_fee_grp/list")
    val actual = allData.is
    S.redirectTo("/taf_t_fee_grp/list", () => allData(actual))

    // use the StatefulSnippet.link method (instead of SHtml.link)

    // Remember to call unregisterThisSnippet() when you are finished with your workflow
  }

  case class TafFeeGroupField (dbField: Field, value: String) {
     /** make a part of a where clause for type String */
     def str: String = " and " + value + comp + "'" + asStr + "'"

     /** make the comparator of the part of the where clause. Either 'like' or '=' */
     def comp: String = {
       if (asStr.contains("%"))    " like "
       else                        " = "
     }

     /** make a 'column is null' where clause part */
     def isnull: String = " and " + value + " " + asStr

     /** replace '*' with '%' */
     def asStr: String = dbField.get.asInstanceOf[String].replace("*", "%")

     /** return "" if empty of a part of a where clause for a string type */
     def is: String = dbField.get match {
       case d:String => { 
         if (asStr.length == 0) ""
         else if (asStr.contains("--")) "" // silently ignore hacking attempt
         else if (asStr.contains(" null")) isnull
         else str
       } 
       case _ => println("Value is not a String"); ""
     }
  }

}


