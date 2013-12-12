package taf 
package snippet 

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import java.util.Date
import taf.lib._
import taf.db._
import Helpers._
import net.liftweb.mapper._
import net.liftweb._
import http._
import net.liftweb.sitemap.Loc._

object TafOrgCrud3 extends TafTAdminOrg  with LongKeyedMetaMapper[TafTAdminOrg]
    with CRUDify[Long,TafTAdminOrg] {
  // disable delete functionality
  override def deleteMenuLoc = Empty
  

  def firstWord = S.??("First")
  def lastWord  = S.??("Last")
  def pageWord = "page"
// select onchange="pageForm('portal=2146493088;;taf.Admin.st;taf.Admin;1311097500104;','BROWSEGROUPS','string_GROUP_NAME','PAGELENGTH','0','0')" 

  def rowsPerPageWord = "rpp"

  override def _showAllTemplate =  
   <lift:crud.all>  
     <table id={showAllId} class={showAllClass}>  
       <thead>  
         <tr>  
           <crud:header_item><th><crud:name/></th></crud:header_item>  
           <th> </th>  
           <th> </th>  
           <th> </th>  
         </tr>  
       </thead>  
       <tbody>  
         <crud:row>  
           <tr>  
             <crud:row_item><td><crud:value/></td></crud:row_item>  
             <td><a crud:view_href=""  >{S.??("View")}</a></td>  
             <td><a crud:edit_href=""  >{S.??("Edit")}</a></td>  
             <td><a crud:delete_href="">{S.??("Delete")}</a></td>  
           </tr>  
         </crud:row>  
       </tbody>  
       <tfoot>  
         <tr>  
           <td colspan="1"><crud:first>{firstWord}</crud:first>
                     &nbsp;<crud:prev>{previousWord}</crud:prev>
           </td>  
           <td colspan="1"><crud:rpp>{rowsPerPageWord}</crud:rpp></td>  
           <td colspan="1"><crud:page>{pageWord}</crud:page></td>  
           <td colspan="1"><crud:next>{nextWord}</crud:next></td>  
           <td colspan="2"><crud:last>{lastWord}</crud:last></td>  
         </tr>  
       </tfoot>  
     </table>  
   </lift:crud.all>  

  protected def crudAllFirst(first: Long)(in: NodeSeq) = 
    if (first < rowsPerPage) <xml:group>&nbsp;</xml:group>
    else <a href={listPathString+"?first="+(0L)}>{in}</a>

  protected def crudAllLast()(in: NodeSeq) = 
    <a href={listPathString+"?first="+(count() - rowsPerPage)}>{in}</a>

  // TODO: java format string page {0} of {1}
  protected def crudAllPage(first: Long, list: List[TheCrudType])(in: NodeSeq) = 
    <span>{S.??("Page") + " " + (first / rowsPerPage + 1) + " " + S.??("of") + " " + (count() / rowsPerPage + 1)}</span>

  //      <!-- {listPathString+"?first="+first} -->
  protected def crudAllRowsPerPage(first: Long)(in: NodeSeq) = 
	<select size="1" name="comboSize">
	<option>3</option>
	<option selected="true">5</option>
	<option>7</option>
	<option>10</option>
	<option>15</option>
	<option>20</option>
	<option>30</option>
	<option>50</option>
	<option>99</option>
	</select>

  override def doCrudAll(in: NodeSeq): NodeSeq = {
    val first = S.param("first").map(toLong) openOr 0L
    val list = findForList(first, rowsPerPage)

    val bundles = S.resourceBundles
    //bundles.map(_ println)
    //import java.util.Locale
    //import net.liftweb._
    //println ("the bundles: " + bundles.map({bu : BundleBuilder => bu.convert(in, Locale.GERMANY)}))
    
    bind("crud", in, "header_item" -> doCrudAllHeaderItems _,
         "row"  -> doCrudAllRows(list) _,
         "first"  -> crudAllFirst(first) _, 
         "last"   -> crudAllLast() _, 
         "rpp"    -> crudAllRowsPerPage(first) _,
         "page"   -> crudAllPage(first, list) _,
         "prev"   -> crudAllPrev(first) _, 
         "next"   -> crudAllNext(first, list) _
    )

//         FuncAttrBindParam("Page", {in: NodeSeq  =>  
//             Text("page " + first + " of " + (first + list.length)    
//                  )},"span")

        
  }


}