/*
 * Copyright 2006-2011 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package common
package db

import net.liftweb._
import http._
import util._
import common._
import Helpers._

import scala.xml._
import http.js.jquery.JqJsCmds.DisplayMessage 
import http.js._

trait TableComponentAjax {
  /**
   * The type of records we're manipulating
   */
  type TheCrudType

  /**
   * A generic representation of a field.  For example, this represents the
   * abstract "name" field and is used along with an instance of TheCrudType
   * to compute the BaseField that is the "name" field on the specific instance
   * of TheCrudType
   */
  type FieldPointerType

  /**
   * This trait represents a Bridge between TheCrudType
   * and the Crudify trait.  It's not necessary to mix this
   * trait into TheCrudType, but instead provide a mechanism
   * for promoting a TheCrudType to CrudBridge
   */
  protected trait CrudBridge {
    /**
     * Delete the instance of TheCrudType from the backing store
     */
    def delete_! : Boolean

    /**
     * Save an instance of TheCrudType in backing store
     */
    def save : Boolean

    /**
     * Validate the fields in TheCrudType and return a List[FieldError]
     * representing the errors.
     */
    def validate: List[FieldError]

    /**
     * Return a string representation of the primary key field
     */
    def primaryKeyFieldAsString: String
  }

  /**
   * This method will instantiate a bridge from TheCrudType so
   * that the appropriate logical operations can be performed
   * on TheCrudType
   */
  protected implicit def buildBridge(from: TheCrudType): CrudBridge

  protected trait FieldPointerBridge {
    /**
     * What is the display name of this field?
     */
    def displayHtml: NodeSeq
  }

  /**
   * Based on a FieldPointer, build a FieldPointerBridge
   */
  protected implicit def buildFieldBridge(from: FieldPointerType): FieldPointerBridge
  
  lazy val Prefix = calcPrefix
  lazy val ListItems = calcListItems
  lazy val CreateItem = calcCreateItem
  lazy val EditItem = calcEditItem
  lazy val DeleteItem = calcDeleteItem

  /**
   * What's the prefix for this CRUD.  Typically the table name
   */
  def calcPrefix: List[String]

  /**
   * Vend a new instance of TheCrudType
   */
  def create: TheCrudType

  def calcListItems = "list"

  def calcCreateItem = "create"

  def calcEditItem = "edit"

  def calcDeleteItem = "delete"

  def displayName = displayHtml.text

  def displayHtml: NodeSeq = Text(calcPrefix.head)

  /**
  * The fields displayed on the list page.  By default all
  * the displayed fields, but this list
  * can be shortened.
  */
  def fieldsForList: List[FieldPointerType] = fieldsForDisplay

  /**
   * When displaying a record, what fields do we display
   */
  def fieldsForDisplay: List[FieldPointerType]

  /**
   * The list of fields to present on a form form editting
   */
  def fieldsForEditing: List[FieldPointerType] = fieldsForDisplay

  /** Still used ? Ho */
  def pageWrapper(body: NodeSeq): NodeSeq =
  <lift:surround with="default" at="content">
    {
      body
    }
  </lift:surround>



  /**
   * Customize the display of a row for displayRecord
   */
  protected def doDisplayRecordRow(entry: TheCrudType)(in: NodeSeq): NodeSeq =
    for {
      pointer <- fieldsForDisplay
      field <- computeFieldFromPointer(entry, pointer).toList
      if field.shouldDisplay_?
      node <- bind("crud", in, 
                   "name" -> field.displayHtml, 
                   "value" -> field.asHtml)
    } yield node
  
  /**
   * Customize the display of records for view menu loc
   */
  protected def displayRecord(entry: TheCrudType)(in: NodeSeq): NodeSeq = {
    
    bind("crud", in, "row" -> doDisplayRecordRow(entry) _)
  }



  /**
   * This is the template that's used to render the page after the
   * optional wrapping of the template in the page wrapper
   */
  def editTemplate(): NodeSeq = pageWrapper(_editTemplate)

  def editId = "edit_page"
  def editClass = "edit_class"
  def editErrorClass = "edit_error_class"

  /**
   * The core template for editting.  Does not include any
   * page wrapping
   */
  protected def _editTemplate = {
  <lift:crud.edit form="post">
    <table id={editId} class={editClass}>
      <crud:field>
        <tr>
          <td>
            <crud:name/>
          </td>
          <td>
            <crud:form/>
          </td>
        </tr>
      </crud:field>

      <tr>
        <td>&nbsp;</td>
        <td><crud:submit>{editButton}</crud:submit></td>
      </tr>
    </table>
  </lift:crud.edit>
  }

  def editButton = S.??("Save")

  /**
   * Override this method to change how fields are displayed for delete
   */
  protected def doDeleteFields(item: TheCrudType)(html: NodeSeq): NodeSeq =
    for {
      pointer <- fieldsForDisplay
      field <- computeFieldFromPointer(item, pointer).toList
      if field.shouldDisplay_?
      node <- bind("crud", html, 
                   "name" -> field.displayHtml,
                   "value" -> field.asHtml)
    } yield node
  
  /**
   * Override this method to change the behavior of deleting an item
   */
  protected def doDeleteSubmit(item: TheCrudType, from: String)() = {
    S.notice(S ? "Deleted")
    item.delete_!
    S.redirectTo(from)
  }



  /**
   * Override this method to change how the delete screen is built
   */
  protected def crudyDelete(item: TheCrudType)(html: NodeSeq): NodeSeq = {
    val from = referer
    
    bind("crud", html,
         "field" -> doDeleteFields(item) _,
         "submit" ->
         ((text: NodeSeq) => SHtml.submit(text.text, 
                                          doDeleteSubmit(item, from) _)))
  }



  private def hasParamFor(pp: ParsePath, toTest: List[String]): Boolean = {
    pp.wholePath.startsWith(toTest) &&
    pp.wholePath.length == (toTest.length + 1) &&
    findForParam(pp.wholePath.last).isDefined
  }

  /**
   * This is the template that's used to render the page after the
   * optional wrapping of the template in the page wrapper
   */
  def deleteTemplate(): NodeSeq = pageWrapper(_deleteTemplate)

  def deleteId = "delete_page"
  def deleteClass = "delete_class"

  /**
   * The core template for deleting.  Does not include any
   * page wrapping
   */
  def _deleteTemplate =
  <lift:crud.delete form="post">
    <table id={deleteId} class={deleteClass}>
      <crud:field>
        <tr>
          <td>
            <crud:name/>
          </td>
          <td>
            <crud:value/>
          </td>
        </tr>
      </crud:field>

      <tr>
        <td>&nbsp;</td>
        <td><crud:submit>{deleteButton}</crud:submit></td>
      </tr>
    </table>
  </lift:crud.delete>

  def deleteButton = S.??("Delete")


  def createMenuName = S.??("Create")+" "+displayName

  /**
   * This is the template that's used to render the page after the
   * optional wrapping of the template in the page wrapper
   */
  def createTemplate(): NodeSeq = pageWrapper(_createTemplate)

  def createId = "create_page"
  def createClass = "create_class"

  /**
   * The core template for creating.  Does not include any
   * page wrapping
   */
  def _createTemplate =
  <lift:crud.create form="post">
    <table id={createId} class={createClass}>
      <crud:field>
        <tr>
          <td>
            <crud:name/>
          </td>
          <td>
            <crud:form/>
          </td>
        </tr>
      </crud:field>

      <tr>
        <td>&nbsp;</td>
        <td><crud:submit>{createButton}</crud:submit></td>
      </tr>
    </table>
  </lift:crud.create>

  def createButton = S.??("Create")


  def showAllMenuName = S.??("List")+" "+displayName

  /**
   * This is the template that's used to render the page after the
   * optional wrapping of the template in the page wrapper
   */
  def showAllTemplate(): NodeSeq = pageWrapper(_showAllTemplate)

  def showAllId = "show_all"
  def showAllClass = "show_all"

  /**
   * The core template for showing record.  Does not include any
   * page wrapping
   *
   */
  def _showAllTemplate =
    <form id="tab1">
    <table id={showAllId} class={showAllClass}>
      <thead>
        <tr>
          <crud:header_item><th><crud:name/></th></crud:header_item>
          <th>&nbsp;</th>
          <th>&nbsp;</th>
        </tr>
      </thead>
      <tbody>
        <crud:row>
          <tr>
            <crud:row_item><td><crud:value/></td></crud:row_item>
            <td><a crud:edit_href="">{S.??("Edit")}</a></td>
            <td><a crud:delete_href="">{S.??("Delete")}</a></td>
          </tr>
        </crud:row>
      </tbody>
      <tfoot>
        <tr>
          <td colspan="99">
             <crud:first>&lt;&lt;</crud:first>&nbsp;
             <crud:prev>&lt;</crud:prev>&nbsp;
             <crud:rpp>{ajaxPprSnippet}</crud:rpp>
             <crud:next>&gt;</crud:next>&nbsp;
             <crud:last>&gt;&gt;</crud:last>
             <crud:search>search</crud:search>
          </td>
        </tr>
        <tr>
          <td colspan="99">	Export as <a crud:clip_href="">text</a> to clipboard or 
             <a crud:file_href="">csv</a> to file</td>
        </tr>
      </tfoot>
    </table>
    </form>

  lazy val listPath = Prefix ::: List(ListItems)

//  lazy val listPathString: String = mp(listPath)
  lazy val listPathString: String = mp(S.request.map(_.path.wholePath) openOr listPath)

  lazy val createPath = Prefix ::: List(CreateItem)

  lazy val createPathString: String = mp(createPath)

  lazy val editPath = Prefix ::: List(EditItem)

  lazy val editPathString: String = mp(editPath)

  lazy val deletePath = Prefix ::: List(DeleteItem)

  lazy val deletePathString: String = mp(deletePath)

  private def mp(in: List[String]) = in.mkString("/", "/", "")


  /**
   * Given a range, find the records.  Your implementation of this
   * method should enforce ordering (e.g., on primary key)
   */
  def findForList(start: Long, count: Int): List[TheCrudType] 

  /**
   * Given a String that represents the primary key, find an instance of
   * TheCrudType
   */
  def findForParam(in: String): Box[TheCrudType]

  /**
   * Given an instance of TheCrudType and FieldPointerType, convert
   * that to an actual instance of a BaseField on the instance of TheCrudType
   */
  protected def computeFieldFromPointer(instance: TheCrudType, pointer: FieldPointerType): Box[BaseField]

  /**
   * This method defines how many rows are displayed per page.  By
   * default, it's hard coded at 20, but you can make it session specific
   * or change the default by overriding this method
   */
  protected def rowsPerPage: Int = 5

  /**
   * The content of the 'rows per page' drop down box. 
   * Something like (2, 5, 10 ...)
   */
  protected def rowsPerPageOption = List(2, 5, 10, 20, 50, 100, 200, 500).map(i => (i.toString, i.toString))
      
  /**
   * Override this method to customize how header items are treated
   */
  protected def doCrudAllHeaderItems(in: NodeSeq): NodeSeq =
    fieldsForList.flatMap(f =>
      bind("crud", in, "name" -> f.displayHtml))
    
  /**
   * Override this method to customize how a crudAll line is generated
   */
  protected def doCrudAllRowItemXXX(c: TheCrudType)(in: NodeSeq): NodeSeq =
    for {
      pointer <- fieldsForList
      field <- computeFieldFromPointer(c, pointer).toList
      node <- bind("crud", in, "value" -> field.asHtml)
    } yield node
  
  protected def doCrudAllRowItem(c: TheCrudType)(in: NodeSeq): NodeSeq =
    for {
      pointer <- fieldsForList
      field <- computeFieldFromPointer(c, pointer).toList
      node <- bind("crud", in, 
        "value" -> <a href={listPathString+"?field="+field.name+"&val="+field.asHtml}>{field.asHtml}</a>)
    } yield node
  
  /**
   * Override this method to determine how all the rows on a crud
   * page are displayed
   */
  protected def doCrudAllRows(list: List[TheCrudType])(in: NodeSeq): NodeSeq =
    list.take(rowsPerPage).flatMap{
      // the first item should be a link and the link should point to ...
      // FuncAttrBindParam("productid_href", {html: NodeSeq =>
      //                   Text("pproduct?productid="+ (product.productid))},"href"),
      c =>
      bind("crud", in , "row_item" -> doCrudAllRowItem(c) _,
          // TODO remove that edit... stuff
          
           FuncAttrBindParam("edit_href", { ignore : NodeSeq =>
             Text(editPathString+"/"+(obscurePrimaryKey(c))) },"href"),
           
           FuncAttrBindParam("delete_href", { ignore : NodeSeq =>
             Text(deletePathString+"/"+
                  (obscurePrimaryKey(c)))},"href")
         )}
  

  /**
   * Override this method to change how the previous link is
   * generated
   */
  protected def crudAllPrev(first: Long)(in: NodeSeq) = 
    if (first < rowsPerPage) <xml:group>{in}</xml:group>
    else <a href={listPathString+
                  "?first="+(0L max (first -
                                     rowsPerPage.toLong))}>{in}</a>
  
  /**
   * Override this method to change how the next link is generated
   */
  protected def crudAllNext(first: Long, list: List[TheCrudType])(in: NodeSeq) = 
    if (list.length < rowsPerPage) <xml:group>{in}</xml:group>
    else <a href={listPathString+"?first="+(first +
                                            rowsPerPage.toLong)}>{in}</a>
  
//  protected def crudAllRowsPerPage(rpp: Long)(in: NodeSeq) = 
//    SHtml.select(rowsPerPageOption,
//        Full(rpp.toString),
//        v => onPprChange(v, in))
//        
//  protected def onPprChange(v: String, in: NodeSeq) {
//    println("combo box ppr updated: " + v)
//  }
  
  protected def ajaxPprSnippet() =
                <ajax:select>
                    You selected <sel:number></sel:number> From the select box.
                </ajax:select>
  
  protected def crudAllRowsPerPage(rpp: Long)(in: NodeSeq) = 
    SHtml.ajaxSelect(rowsPerPageOption,
        Full(rpp.toString),
        v => onPprChange(v, in))
        
  protected def onPprChange(v: String, in: NodeSeq) {
    println("combo box ppr updated: " + v)
    rpp = v.toInt
    DisplayMessage("messages",
                                   bind("sel", in, "number" -> Text(v)),
                                   5 seconds, 1 second)
    // submitAjaxForm (formId: String, postSubmit: Call) : JsCmd 
    SHtml.submitAjaxForm("tab1", () => JsCmds._Noop)   
  }
                                     
//  protected def onPprChangeAAA(t: String) {
//    println("combo box ppr updated: " + t)
//    JsCmds._Noop
//  }
        
  protected def crudAllRowsPerPageStatic(first: Long)(in: NodeSeq) = 
	<select size="1" name="comboSize">
	<option>2</option>
	<option selected="true">5</option>
	<option>10</option>
	<option>20</option>
	<option>50</option>
	<option>100</option>
	<option>200</option>
	<option>500</option>
	</select>

  protected def crudSearch(text: String)(in: NodeSeq) = 
	<input type="text" style="width:100px" value={text} name="inputSeek">Search</input>

  // Export
  protected def doExpFunc(toExport: String, expType: String, theRest: String, html: NodeSeq) =
                 FuncAttrBindParam(expType + "_href", {html: NodeSeq =>
                   Text("../common/export?toExport=" + toExport + "&expType=" + expType + "&expText=" + theRest)},"href")


  var rpp = rowsPerPage;
  
  /**
   * Override this method if you want to change the behavior
   * of displaying records via the crud.all snippet
   */
  protected def doCrudAll(ignore: NodeSeq): NodeSeq = {
    val first = S.param("first").map(toLong) openOr 0L
    val list = findForList(first, rowsPerPage)
    //val defLen: Long = S.param("comboSize").map(toLong) openOr list.length.toLong
    val searchText = S.param("inputSeek").map(String.valueOf(_)) openOr ""
    
    // TEST
    println("referer: " + referer)
    println("list path string: " + listPathString)
    println("path whole mkString: " + S.request.map(_.path.wholePath.mkString("A", "B", "C")))

    val in = _showAllTemplate
    
    val ns: NodeSeq =
    bind("crud", in,
         "header_item" -> doCrudAllHeaderItems _,
         "row"  -> doCrudAllRows(list) _,
         "first"-> crudAllPrev(0) _,
         "prev" -> crudAllPrev(first) _,
         "next" -> crudAllNext(first, list) _,
         "last" -> crudAllNext(first, list) _,
         "rpp"  -> crudAllRowsPerPage(rpp) _,
         "search"  -> crudSearch("") _,
                 doExpFunc(Prefix(0), "clip", "", in),
                 doExpFunc(Prefix(0), "file", "", in)         
    )

    // TEST
    //println("doCrudAll: ns: " + ns)
    ns
  }

  
  
/*
// submit search if user presses ENTER in the search input field
function pageForm2(portal, tableId, sortedBy, functId, topRow, rows, rowId, rowNum, colId)
{
   var e = window.event;

   if (e.keyCode == 13) 
   {
      // ENTER
      pageForm(portal, tableId, sortedBy, functId, topRow, rows, rowId, rowNum, colId);  
   }
}

function pageForm(portal, tableId, sortedBy, functId, topRow, rows, rowId, rowNum, colId)
{
   var seekVal = "";
   var canSeek = document.TableBrowser.canSeek.value;
   if (document.TableBrowser.inputSeek)
   {
     seekVal = document.TableBrowser.inputSeek.value;
   }
   
   var pageLen = document.TableBrowser.pageLen.value;
   var canSetLen = document.TableBrowser.canSetLen.value;
   if (document.TableBrowser.comboSize)
   {
     var sizeSel = document.TableBrowser.comboSize.selectedIndex;
     if(sizeSel > -1)
     {
        pageLen  = document.TableBrowser.comboSize[sizeSel].text;
     }
   }

   var selFirstRow = document.TableBrowser.selectFirstRow.value;
   var eventId = tableId;
   if(functId == tableId + "_ITEM")
   {
      eventId  = tableId + "_ITEM";
   }

   var actPara = portal  + "&tableId="     + tableId 
                         + "&eventId="     + eventId  
                         + "&FUNCTION="    + functId 
                         + "&SORTEDBY="    + sortedBy 
                         + "&PAGESEEK="    + seekVal 
                         + "&CANSEEK="     + canSeek 
                         + "&PAGELEN="     + pageLen
                         + "&CANSETLEN="   + canSetLen
                         + "&SELFIRSTROW=" + selFirstRow
                         + "&TOPROW="      + topRow  
                         + "&ROWS="        + rows     
                         + "&COLID="       + colId    
                         + "&ROWID="       + rowId    
                         + "&ROWNUM="      + rowNum;

   document.TableBrowser.action = "enterWorkflow.do?" + actPara;
   document.TableBrowser.submit();
}
 */
  
  /**
   * This method can be used to obscure the primary key.  This is more secure
   * because end users will not have access to the primary key.
   */
  def obscurePrimaryKey(in: TheCrudType): String = obscurePrimaryKey(in.primaryKeyFieldAsString)

  /**
   * This method can be used to obscure the primary key.  This is more secure
   * because end users will not have access to the primary key.  This method
   * actually does the obfuscation.  You can use Mapper's KeyObfuscator class
   * to implement a nice implementation of this method for session-by-session
   * obfuscation.<br/><br/>
   *
   * By default, there's no obfuscation.  Note that if you obfuscate the
   * primary key, you need to update the findForParam method to accept
   * the obfuscated keys (and translate them back.)
   */
  def obscurePrimaryKey(in: String): String = in

  def referer: String = S.referer openOr listPathString

  /**
   * As the field names are being displayed for editting, this method
   * is called with the XHTML that will be displayed as the field name
   * an a flag indicating that the field is required (or not).  You
   * can wrap the fieldName in a span with a css class indicating that
   * the field is required or otherwise do something to update the field
   * name indiciating to the user that the field is required.  By default
   * the method wraps the fieldName in a span with the class attribute set
   * to "required_field"
   */
  def wrapNameInRequired(fieldName: NodeSeq, required: Boolean): NodeSeq = {
    if (required) {
      <span class="required_field">{fieldName}</span>
    } else {
      fieldName
    }
  }

  def crudDoForm(item: TheCrudType, noticeMsg: String)(in: NodeSeq): NodeSeq = {
    val from = referer
    val snipName = S.currentSnippet

    def loop(html:NodeSeq): NodeSeq = {
      def error(field: BaseField): NodeSeq = 
        field.uniqueFieldId match {
	  case fid @ Full(id) => S.getNotices.filter(_._3 == fid).flatMap(err =>
	    List(Text(" "), <span class={editErrorClass}>{err._2}</span>) )

	  case _ => NodeSeq.Empty
	}

      def doFields(html: NodeSeq): NodeSeq =
        for {
          pointer <- fieldsForEditing
          field <- computeFieldFromPointer(item, pointer).toList
          if field.show_?
          form <- field.toForm.toList
          node <- bind("crud", html,
                       "name" -> (wrapNameInRequired(field.displayHtml,
                                                    field.required_?) ++
                                  error(field)),
                       "form" -> form)
        } yield node

      def doSubmit() = item.validate match {
        case Nil =>
          S.notice(noticeMsg)
          item.save
          S.redirectTo(from)

        case xs =>
          S.error(xs)
          snipName.foreach(S.mapSnippet(_, loop))
      }

      bind("crud", html,
           "field" -> doFields _,
           "submit" ->
           ((text: NodeSeq) => SHtml.submit(text.text, doSubmit _)))
    }

    loop(in)
  }


}

