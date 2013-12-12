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

trait TableComponent {
  val log = Logger(getClass().getName())

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
  <form id="tab2" method="post">
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
  </form>
  }

  def editButton = S.??("Save")

  /**
   * Override this method to change the behavior of deleting an item
   */
  protected def doDeleteSubmit(item: TheCrudType, from: String)() = {
    S.notice(S ? "Deleted")
    item.delete_!
    S.redirectTo(from)
  }


  private def hasParamFor(pp: ParsePath, toTest: List[String]): Boolean = {
    pp.wholePath.startsWith(toTest) &&
    pp.wholePath.length == (toTest.length + 1) &&
    findForParam(pp.wholePath.last).isDefined
  }


  //def showAllMenuName = S.??("List")+" "+displayName

  /**
   * This is the template that's used to render the page after the
   * optional wrapping of the template in the page wrapper
   */
  def showAllTemplate(): NodeSeq = pageWrapper(_showAllTemplate)

  def showAllId = "show_all"
  def showAllClass = "show_all"
  def showAllFormId = "show_all_form"
  def onclick = "tableForm('" + showAllFormId + "')"
    
  /**
   * The core template for showing record.  Does not include any
   * page wrapping
   *
   */
  def _showAllTemplate =
    <form id={showAllFormId} method="post">
    <!--<input type="hidden" name="tableData" crud:tableData=""></input>-->
    <crud:tableData/>
    <table id={showAllId} class={showAllClass}>
      <thead>
        <tr>
          <crud:header_item><th crud:wid="" crud:cls=""><crud:name/></th></crud:header_item>
        </tr>
      </thead>
      <tbody>
        <crud:row>
          <tr>
            <crud:row_item><td><crud:value/></td></crud:row_item>
          </tr>
        </crud:row>
      </tbody>
      <tfoot>
        <tr>
          <td colspan="99">
             <crud:first>&lt;&lt;</crud:first>&nbsp;
             <crud:prev>&lt;</crud:prev>&nbsp;
             <crud:rpp>rowsPerPage</crud:rpp>
             <crud:next>&gt;</crud:next>&nbsp;
             <crud:last>&gt;&gt;</crud:last>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
             <crud:count>count</crud:count>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
             <crud:search>search</crud:search>&nbsp;<a href="#" onclick={onclick}>{S.??("Search")}</a>
          </td>
        </tr>
        <tr>
          <td colspan="99">	Export as <a crud:clip_href="">text</a> to clipboard or 
             <a crud:file_href="">csv</a> to file</td>
        </tr>
      </tfoot>
    </table>
    </form>
            
  // TODO if row-count < rowsPerPage print 'rows = n'            

  lazy val listPath = Prefix ::: List(ListItems)

//  lazy val listPathString: String = mp(listPath)
  lazy val basePath = S.request.map(_.path.wholePath) openOr listPath
  lazy val listPathString: String = mp(basePath)

  lazy val createPath = Prefix ::: List(CreateItem)

  lazy val createPathString: String = mp(createPath)

  lazy val editPath = basePath.dropRight(1)

  protected lazy val editPathString: String = mp(editPath) + "/edit"

  lazy val deletePath = Prefix ::: List(DeleteItem)

  lazy val deletePathString: String = mp(deletePath)

  protected def mp(in: List[String]) = in.mkString("/", "/", "")


  /**
   * Given a range, find the records.  Your implementation of this
   * method should enforce ordering (e.g., on primary key)
   */
  def findForList(start: Long, count: Int): List[TheCrudType] 

  /**
   * Count the elements the 'findForList' can return.
   * If the result is -1 we will not display that 'count' information to the user
   */
  def countForList: Long

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

  /** The store of this table */
  lazy val td = new TableData
  
  /** For efficiency we count the rows only once until the 'search' changes */
  var lastCount = -1L
  
  /**
   * This method defines how many rows are displayed per page.  By
   * default, it's hard coded at 20, but you can make it session specific
   * or change the default by overriding this method
   */
  protected def rowsPerPage: Int = td.rowsPerPage

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
      bind("crud", in, 
          "name" -> forEachHeaderHead(f), /*f.displayHtml,*/
           FuncAttrBindParam("wid", {in: NodeSeq =>
                   Text(forEachHeaderWid(f))},"width"),
           FuncAttrBindParam("cls", {in: NodeSeq =>
                   Text(forEachHeaderClass(f))},"class")
      )
    )
    
  protected def forEachHeaderWid(f: FieldPointerType) = "300"
    
  protected def forEachHeaderHead(f: FieldPointerType) = {
    val label = f.displayHtml.toString.capitalize
    forEachHeaderSortable(f) match {
      case true => <a href={listPathString+"?col="+f.displayHtml}>{label}</a>
      case false => <span>{label}</span>
    }
  }

  protected def forEachHeaderClass(f: FieldPointerType) = {
    (f.displayHtml.toString.equals(selectedColumn)) match {
      case true =>  "tableBrowserHeaderSelected"
      case false => "tableBrowserHeader"
    }
  }
  
  protected def forEachHeaderSortable(f: FieldPointerType) = true
    
  protected def doCrudAllRowItem(c: TheCrudType)(in: NodeSeq): NodeSeq =
    for {
      pointer <- fieldsForList
      field <- computeFieldFromPointer(c, pointer).toList
      node <- bind("crud", in, 
        "value" -> forEachRow(field))
        //"value" -> <a href={listPathString+"?field="+field.name+"&rowsel="+i}>{field.asHtml}</a>)
    } yield node
  
  protected def forEachRow(field: BaseField) = {
    forEachFieldSelectable(field) match {
      case true  => <a href={editPathString+"?field="+field.name+"&val="+field.asHtml}>{field.asHtml}</a>
      case false => <span>{field.asHtml}</span>
    }
  }
    
  protected def forEachFieldSelectable(field: BaseField) = field.name.equals("productid")
    
  /**
   * Override this method to determine how all the rows on a crud
   * page are displayed
   */
  protected def doCrudAllRows(list: List[TheCrudType])(in: NodeSeq): NodeSeq =
    list.take(rowsPerPage).flatMap{ c =>
      bind("crud", in , "row_item" -> doCrudAllRowItem(c) _ )}
  

  /**
   * Override this method to change how the previous link is
   * generated
   */
  protected def crudAllPrev(first: Long)(in: NodeSeq) = 
    if (first < rowsPerPage) <xml:group>{in}</xml:group>
    else crudAllFirst(first)(in)
  
  /**
   * Override this method to change how the first link is
   * generated
   */
  protected def crudAllFirst(first: Long)(in: NodeSeq) = 
    <a href={listPathString+
                  "?first="+(0L max (first -
                                     rowsPerPage.toLong))}>{in}</a>
  
  /**
   * Override this method to change how the next link is generated
   */
  protected def crudAllNext(first: Long, rows: Long)(in: NodeSeq) = 
    if (rows < rowsPerPage) <xml:group>{in}</xml:group>
    else <a href={listPathString+"?first="+(first +
                                            rowsPerPage.toLong)}>{in}</a>
  
  // select box 'Rows per page'
  protected def crudAllRowsPerPage(rpp: Long)(in: NodeSeq) = 
    SHtml.select(rowsPerPageOption,
        Full(rpp.toString),
        v => onPprChange(v, in),
        SHtml.ElemAttr.pairToBasic("onchange", "submit()")
      )
        
  protected def onPprChange(v: String, in: NodeSeq) {
    log.debug("combo box ppr updated: " + v)
    td.rowsPerPage = v.toInt
  }
  
  protected def crudAllCount(index: Long, rows: Long, rpp: Long)(in: NodeSeq) = {
    val total = lastCount //20L // countForList
    val last  = index + rows
    val first = index + 1
    val page  = first / rpp + 1
    val pages = total / rpp + 1 
    Text(S.??("Rows") + ": " + first + "-" + last + "/" + total + " ... " + S.??("Pages") + ": " + page + "/" + pages)
  }

  //<a href='#' onclick={onclick}>{S.??("Count")}</a>

  protected def crudAllSearch(text: String)(in: NodeSeq) = 
	<input type="text" style="width:100px" value={text} name="search"></input>

  // Export
  protected def doExpFunc(toExport: String, expType: String, theRest: String, html: NodeSeq) =
                 FuncAttrBindParam(expType + "_href", {html: NodeSeq =>
                   Text("../common/export?toExport=" + toExport + "&expType=" + expType + "&expText=" + theRest)},"href")

  /**
   * Override this method if you want to change the behavior
   * of displaying records via the crud.all snippet
   */
  protected def doCrudAll(ignore: NodeSeq): NodeSeq = {
    //val first = S.param("first").map(toLong) openOr 0L
    //val defLen: Long = S.param("comboSize").map(toLong) openOr list.length.toLong
    //val searchText = S.param("inputSeek").map(String.valueOf(_)) openOr ""
    
    // TEST
    log.debug("referer: " + referer)
    log.debug("list path string: " + listPathString)
    //log.debug("path whole mkString: " + S.request.map(_.path.wholePath.mkString("A", "B", "C")))
    
    td.init
    if (lastCount == -1L || td.isNewSearch)
      lastCount = countForList
    // set default sorting column
    if (td.col.isEmpty) td.col = defaultSortingCol
//    if (td.col.isEmpty) td.col = "name"
    val first = td.first
    val list = findForList(first, rowsPerPage)
    val rows = list.length
    val in = _showAllTemplate
    
    register(list)
    
    bind("crud", in,
         "tableData"   -> td.store _,
         "header_item" -> doCrudAllHeaderItems _,
         "row"    -> doCrudAllRows(list) _,
         "first"  -> crudAllFirst(0) _,
         "prev"   -> crudAllPrev(first) _,
         "next"   -> crudAllNext(first, rows) _,
         "last"   -> crudAllNext(lastCount - td.rowsPerPage, rows) _,
         "rpp"    -> crudAllRowsPerPage(td.rowsPerPage) _,
         "count"  -> crudAllCount(first, rows, td.rowsPerPage) _,
         "search" -> crudAllSearch(td.search) _,
         doExpFunc(Prefix(0), "clip", "", in),
         doExpFunc(Prefix(0), "file", "", in)        
    )

  }

  /** register the displayed data in the store for export as text or csv */
  def register(data: List[TheCrudType])
  
  /** User has selected this column for sorting and searching */
  def selectedColumn = {if (td.col.isEmpty) td.col = defaultSortingCol ; td.col}
  
  // TODO use Box
  def selectedSearch = td.search

  def defaultSortingCol = "nothing"


  // ############ EDIT ###########
  protected def doCrudEdit(ignore: NodeSeq): NodeSeq = {
    //val first = S.param("first").map(toLong) openOr 0L
    //val defLen: Long = S.param("comboSize").map(toLong) openOr list.length.toLong
    //val searchText = S.param("inputSeek").map(String.valueOf(_)) openOr ""
    // take the value 'val' from the request 
    // or use the value '1'
    // or take the first row in the table
    val id = S.param("val").map(String.valueOf(_)) openOr "1"
    val selected = findForParam(id) openOr findForList(0, 1)(0)
    val in: NodeSeq = _editTemplate
    crudDoForm(selected, S.??("Save"))(in)
  }

    
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

