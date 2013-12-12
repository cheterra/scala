package taf 
package snippet 

import scala.xml.{NodeSeq}
//import net.liftweb.mapper._
//import net.liftweb.util._
import net.liftweb._
import util._
import Helpers._
//import mapper._
import http._

trait TafCrudify extends net.liftweb.proto.Crudify {

  def count(): Long

  def firstWord = S.?("First")
  def lastWord  = S.?("Last")
  def pageWord = "page"

  abstract override def _showAllTemplate =
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
           <td colspan="9"><crud:first>{firstWord}</crud:first>
                     &nbsp;<crud:prev>{previousWord}</crud:prev>
                     &nbsp;<crud:page>{pageWord}</crud:page>
                     &nbsp;<crud:next>{nextWord}</crud:next>
                     &nbsp;<crud:last>{lastWord}</crud:last>
           </td>  
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
  // TODO that count() does not work since it counts the whole
  //   table but ignores the where clause
  protected def crudAllPage(first: Long, list: List[TheCrudType])(in: NodeSeq) = 
    <span>{S.?("Page") + " " + (first / rowsPerPage + 1) + " " + S.?("of") + " " + (count() / rowsPerPage + 1)}</span>

  override def doCrudAll(in: NodeSeq): NodeSeq = {
    val first = S.param("first").map(toLong) openOr 0L
    val list = findForList(first, rowsPerPage)

    bind("crud", in, "header_item" -> doCrudAllHeaderItems _,
         "row"  -> doCrudAllRows(list) _,
         "first"  -> crudAllFirst(first) _, 
         "last"   -> crudAllLast() _, 
         "page"   -> crudAllPage(first, list) _,
         "prev"   -> crudAllPrev(first) _, 
         "next"   -> crudAllNext(first, list) _
    )

  }

}