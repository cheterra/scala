package taf
package snippet

import net.liftweb._
import http._
import mapper._
import taf.db._

/**
 * Declare the fields on the screen
 */
object ScreenSelectFeeTableSmall extends LiftScreen {
  val ag =    myField("Agency",         Person.agencyno.dbColumnName, "009603")
  val bu =    myField("Business Unit",  Person.businessunitno.dbColumnName, "")
  val cust =  myField("Customer",       Person.customerno.dbColumnName, "*")

  var persList =   List(ag, bu, cust)
  var officeList = List(ag, bu)

  def myField(name: String, acc: String, deflt: String) = new TafField(field(S.?(name), deflt), acc)

  object personReq extends RequestVar(List(ag.get))
  object officeReq extends RequestVar(List(ag.get))
  object all       extends RequestVar(Map("x" ->List(ag.get)))

// request: Full(
//   Req(
//     List(F7494204184320GQMAT, F749420368428VZ3BWU, F749420378427DAZZGI, F749420378425PBLUUN, F7494203784344T3XE1, F749420378426VX1RCC)
//       , Map(
//         F7494203784344T3XE1 -> List(true)
//         , F749420368428VZ3BWU -> List(true)
//         , F749420378426VX1RCC -> List()
//         , F749420378425PBLUUN -> List(009603)
//         , F749420378427DAZZGI -> List(*)
//         , F7494204184320GQMAT -> List(true))
//       , ParsePath(List(taf, screenSelectFeeGrpSearchCriteria),,true,false)
//       , 
//       , PostRequest, Full(application/x-www-form-urlencoded)
//   ))

  def finish() {
    println("request: " + S.request)
    //val fullURI = S.request.map(_.request.getRequestURI) openOr ("Undefined")
    //println("full URI: " + fullURI)
    println("uri: " + S.uri)
    println("location: " + S.location)
    println("hostAndPath: " + S.hostAndPath)
    println("html: " + S.htmlProperties)
    println("header: " + S.forHead)
    println("queryLog: " + S.queryLog)
    println("queryString: " + S.queryString)
    personReq(  persList.map(_.get))
    officeReq(officeList.map(_.get))
    println("person: " + personReq.is)
    val actual: Map[String, List[String]] = Map(
      "person"       -> personReq.is, 
      "office"       -> officeReq.is,
      "agency"       -> List(ag.getValue),
      "businessunit" -> List(bu.getValue),
      "customer"     -> List(cust.getValue)
    )
    println("actual: " + actual)
    S.uri match {
      case "/taf/scope/screenSelectFeeGrpSearchCriteria"  => S.redirectTo("/taf/scope/screenResultFeeGrp", () => all(actual))
      case _                                        => S.redirectTo("/taf/scope/screenResultFeeTableSmall", () => all(actual))
    }

    //S.redirectTo("/taf/screenResultFeeTableSmall", () => all(actual))
  }

}


