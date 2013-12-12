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

import model.StoreVarReq
import common.db._

import net.liftweb._
import mapper._
import http._
import util._
import common._
import Helpers._

import scala.xml._

import net.liftweb._


/**
 * Docu copied from CRUDIfy. But this is now a table component.
 * 
 * This trait automatically adds CRUD (Create, read, update and delete) operations
 * to an existing <b>MetaMapper</b> object. Various methods can be overridden to
 * customize which operations are available to a user and how things are displayed.
 * For example, you can disable deletion of entities by overriding deleteMenuLoc to Empty.
 *
 * Note: Compilation will fail if you try to mix this into a Mapper instead of the
 * associated MetaMapper. You have been warned.
 */
trait TABLEify[KeyType, CrudType <: KeyedMapper[KeyType, CrudType]] extends
  TableComponent with ExportableList {
  self: CrudType with KeyedMetaMapper[KeyType, CrudType] with ExportableList =>
    
  override val log = Logger(getClass().getName())


  /**
   * What's the record type for the underlying CRUDify?
   */
  type TheCrudType = CrudType

  /**
   * What's a field pointer for the underlying CRUDify
   */
  type FieldPointerType = MappedField[_, CrudType]

  /**
   * Given a field pointer and an instance, get the field on that instance
   */
  protected def computeFieldFromPointer(instance: TheCrudType, pointer: FieldPointerType): Box[BaseField] = Full(getActualField(instance, pointer))

  /**
   * Given a String that represents the primary key, find an instance of
   * TheCrudType
   */
  def findForParam(in: String): Box[TheCrudType] = find(in)

  /**
   * Get a List of items from the database
   */
  def findForList(start: Long, count: Int): List[TheCrudType] =
    findAll(StartAt[CrudType](start) :: MaxRows[CrudType](count) ::
          findForListParams :_*)

  /**
   * Count items in the database
   */
  def countForList: Long = count(findForListParams (0))

  /**
   * What are the query parameters?  Default to ascending on primary key
   */
  def findForListParams: List[QueryParam[CrudType]] =
    //List(OrderBy(primaryKeyField, Ascending))
    findForListParams2

  /**
  * The fields to be displayed. By default all the displayed fields,
  * but this list can be shortened.
  */
  def fieldsForDisplay: List[MappedField[_, CrudType]] = 
    mappedFieldsForModel.filter(_.dbDisplay_?)

  /**
   * What's the prefix for this CRUD.  Typically the table name
   */
  def calcPrefix = List(_dbTableNameLC)


  protected class MyBridge(in: CrudType) extends CrudBridge {
    /**
     * Delete the instance of TheCrudType from the backing store
     */
    def delete_! : Boolean = in.delete_!

    /**
     * Save an instance of TheCrudType in backing store
     */
    def save : Boolean = in.save

    /**
     * Validate the fields in TheCrudType and return a List[FieldError]
     * representing the errors.
     */
    def validate: List[FieldError] = in.validate

    /**
     * Return a string representation of the primary key field
     */
    def primaryKeyFieldAsString: String = in.primaryKeyField.toString
  }

  /**
   * This method will instantiate a bridge from TheCrudType so
   * that the appropriate logical operations can be performed
   * on TheCrudType
   */
  protected implicit def buildBridge(from: TheCrudType): CrudBridge =
    new MyBridge(from)

  protected class MyPointer(in: MappedField[_, CrudType]) extends FieldPointerBridge {
    /**
     * What is the display name of this field?
     */
    def displayHtml: NodeSeq = in.displayHtml
  }

  /**
   * Based on a FieldPointer, build a FieldPointerBridge
   */
  protected implicit def buildFieldBridge(from: FieldPointerType): FieldPointerBridge = new MyPointer(from)

  
  
  
  
  
  /** get the 'width' defined in listviewer or "100" */
  override def forEachHeaderWid(f: FieldPointerType) = {
    f match {
      case f: ListView => f.wi.toString
      case _ => "100"
    }    
  }

  /** get the 'sortable' defined in listviewer or 'false' */
  override def forEachHeaderSortable(f: FieldPointerType) = {
    f match {
      case f: ListView => f.sortable
      case _ => false
    }    
  }
  
  protected def listviewer: List[FieldPointerType]
  //def getExportList = listviewer

//  override def forEachFieldSelectable(field: BaseField) = {
//    field match {
//      case f: ListView => f.selectable
//      case _ => false
//    }    
//  }
  
  override def forEachFieldSelectable(field: BaseField): Boolean = {
    val header = listviewer.filter(_.name.equals(field.name))(0)
    header match {
      case f: ListView => f.selectable
      case _ => false
    }    
  }
  
  /**
   * What are the query parameters?  Default to ascending on primary key
   */
  def findForListParams2: List[QueryParam[CrudType]] = {
    // make a list from Box By(...)
    // select a header column and then ...
    val ffn = fieldForName
    ffn.map(s => log.debug("ffn found: " + s.name))

    // TEST
    if (ffn.isEmpty) {
      log.debug("warn: ffn is empty: " + ffn)
      log.debug("cmp " + fieldsForList(0).name + " with " + defaultSortingCol + " = " + fieldsForList(0).name.equals(defaultSortingCol))
      log.debug("cmp " + fieldsForList(1).name + " with " + defaultSortingCol + " = " + fieldsForList(1).name.equals(defaultSortingCol))
    }
      
    if (ffn.isEmpty)
      throw new IllegalArgumentException("'find for name' is empty." + 
          " That means: no column has been selected. Selected column: " + selectedColumn +
          " defaultSortingCol: " + defaultSortingCol +
           ", fields for list: " + fieldsForList.map(_.name))
    
    // return a 'dummy' query param (user has not searched)
    // or a search param build by the the string the user has entered
    val se3: QueryParam[CrudType] = selectedSearch match {
      case "" => Ignore[CrudType]
      case _ => new BySearchBuilder[CrudType](ffn(0).name, selectedSearch).get
    }
    
//    log.debug("se3: " + se3)
    //log.debug("field for name (" + selectedColumn + "): " + ffn + " class: " + ffn.getClass + " " + ffn)
    se3 ::
    ffn.map(f => OrderBy(f, Ascending))
    //log.debug("listviewer names: " + listviewer.map(_.name))
  }
  
  /**
   * filter the fieldsForList and return the selected column
   */
//  def fieldForName: List[MappedField[_,TheCrudType]] =
//    fieldsForList.filter(_.name.equals(selectedColumn))
  def fieldForName: List[MappedField[_,TheCrudType]] = {
    log.debug("ffn: fieldsForList: " + fieldsForList)
    fieldsForList.filter(f => {
      log.debug("ffn: field: " + f + ", name: " + f.name + ", eq? " + f.name.equals(selectedColumn) + ", secol: " + selectedColumn)
      f.name.equals(selectedColumn)     
    })
  }
  
  
  /** Entry point to TableComponent (subclass) */
  def showTable(in: NodeSeq): NodeSeq = doCrudAll(in)
  
  /** for export csv and text */
//  def register(data: List[TheCrudType]) =
//    StoreVarReq.getStore.register(_dbTableNameLC, new DataList(_, data))
  // Don't know why I have to do a cast here. Isn't Scala smart enough to find the 'interface' ExportableList?
  def register(rows: List[TheCrudType]) =
    StoreVarReq.register(_dbTableNameLC, this, rows.asInstanceOf[List[ExportableList]])
    
  //override def defaultSortingCol = primaryKeyField.name
  override def defaultSortingCol = "productid"
    
  def editTable(in: NodeSeq): NodeSeq = doCrudEdit(in: NodeSeq)

}
