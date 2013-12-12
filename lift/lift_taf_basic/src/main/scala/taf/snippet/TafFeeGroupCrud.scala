package taf 
package snippet 

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import taf.lib._
import taf.db._
import Helpers._
import net.liftweb._
import http._
import mapper._


object TafFeeGroupCrud extends TafTFeeGrp  with LongKeyedMetaMapper[TafTFeeGrp]
    with CRUDify[Long,TafTFeeGrp] with TafCrudify {
  // disable delete functionality
  override def deleteMenuLoc = Empty

  /**
   * What are the query parameters?  Default to ascending on primary key
   */
  override def findForListParams: List[QueryParam[TafTFeeGrp]] = {
    println("what are the input data? " + S.attrsFlattenToMap)
    //println("current snippet attrs: " + S.currentAttrs)
    println("current snippet: " + S.currentSnippet)
    //println("notices: " + S.getNotices)
    println("all notices: " + S.getAllNotices)
    //println("function map: " + S.functionMap)
    //println("id messages: " + S.idMessages _)
    //println("get Agency: " + S.get("Agency"))
    //println("get param Agency: " + S.param("Agency"))
    println("get Agency: " + ScreenSelectFeeTable.allData)
    //println("where clause: " + ScreenSelectFeeTable.allData.flatMap( _ ) )
    val buf = new StringBuilder()
    //println("where clause: " + ScreenSelectFeeTable.allData.map( buf.append ) )
    for (d <- ScreenSelectFeeTable.allData ) buf.append(d)
    println("where clause: " + buf)

    println("find all by state P")
    //List(By(TafTFeeGrp.state, "P"), OrderBy(primaryKeyField, Ascending))
    val today = new java.util.Date()
   //List(BySql("state = 'P' and valid_from < ? and valid_to >= ?")IHaveValidatedThisSQL("frank","2011-07-03"), today)
    List(By(TafTFeeGrp.state, "P"), 
         By_<(TafTFeeGrp.validFrom, today), 
         By_>(TafTFeeGrp.validTo, today),
         InRaw(TafTFeeGrp.feeGrpId,
          //"select fee_grp_id from TAF_T_ORG_CUST where AGENCY_NO like '%62' " + buf,
          "select fee_grp_id from TAF_T_ORG_CUST where " + buf.substring(4),               IHaveValidatedThisSQL("frank", "2008-12-03"))
    )
  }

}