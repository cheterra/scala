package portal
package snippet 

import scala.xml.{NodeSeq, Text}
import scala.collection.mutable.{ListBuffer}
import scala.collection.immutable.HashMap
import net.liftweb.util._
//import TimeHelpers._

import net.liftweb.common._
import java.util.Date
import Helpers._

import portal.db._
import net.liftweb._

import mapper._
import util._
import http._

import http.js.jquery.JqJsCmds.DisplayMessage 
import http.js._
import http.js.jquery._ 

import net.liftweb.util.Mailer
import Mailer._


class SelectedProduct {
  val log = Logger(getClass().getName())

  lazy val licenseTypeOptions = getLicenseTypes.map(_.licensetype)
//  var data: Map[String, String] = new HashMap
  
  // aaargh, hardcoded, I hate it. Want to have kind of 'back'
  val parentPage = "screenSearchProduct"
  
  object ProductVar extends RequestVar[Pproduct](toShowProd)
  
  var productid: String = "1"
    
  var lastMailState: String = null
  
  // capture from whence the user came so we
  // can send them back
  private val whence = S.referer openOr "/"

     def show(html: NodeSeq) : NodeSeq = {
	    // we will be here again when the save button has been pressed
	    val save = get("save")
	    log.debug("save: " + save)
	    if (!save.isEmpty) {
	      update
	      sendUpdatesPerEmail(get("productid"), get("name"), get("description"), get("producttype"), get("productlineid"))
	      //forward to referer
	      // remove session data
  	      S.redirectTo(parentPage)
	      return <span></span>
	    }
	    
	    val cancel = get("cancel")
	    log.debug("cancel: " + cancel)
	    if (!cancel.isEmpty) {
          log.debug("whence: " + whence)
  	      S.redirectTo(parentPage)
	      return <span></span>
	    }
	    
      
       val product = ProductVar.is
       //val product = toShowProd
       //ProductVar.set(product)
       val licgrp = toShowLic(product.productid.toString)
       S.set("productid", product.productid.toString)
       bind("item", html,
            "productid"     -> product.productid,
            "name"          -> product.name,
            FuncAttrBindParam("name", {html: NodeSeq =>
                Text(product.name.toString)},"value"),
            FuncAttrBindParam("description", {html: NodeSeq =>
                Text(product.description.toString)},"value"),
            FuncAttrBindParam("productlineid", {html: NodeSeq =>
                Text(product.productlineid.toString)},"value"),
            "producttype"   -> createProductType(product.producttype.toString),
            "emailenable"   -> radiosEmail(0),
            "emaildisable"  -> radiosEmail(1),
            "licensegroup"  -> createLicenseGroup(licgrp.groupid.toString), //groupid = "V"),
            "licensetype"   -> createLicenseType(licgrp.licensetype.toString)  //licensetype="VEI") 
       )
     }
  
  protected def productTypeOption = ("U", "User (Terminal)") :: ("O", "Office (BST)") :: Nil

  def createProductType(ptype: String) = SHtml.select(productTypeOption,
        Full(ptype),
        v => onChange(v, "producttype"))
      
  protected def onChange(v: String, name: String) = {
    log.debug(name + " updated: " + v)
    S.set(name, v)
    //data += name -> v
    // TEST
    //log.debug("filling map: " + data)
  }
        
  protected def emailEnableOption = ("1") :: ("0") :: Nil

//  val radiosEmail = SHtml.radio(emailEnableOption.toList, Full("0"),v => onChange(v))
  val radiosEmail = SHtml.ajaxRadio[String](emailEnableOption.toList, 
      Full(if (hasEmailsEnabled && hasEmails) "1" else "0"), 
      onEmailEnableChange(_) )

//  def onAjaxChange(v: String) {
//    log.debug("ajax updated: " + v)
////    DisplayMessage("messages",
////                                   bind("sel", in, "number" -> Text(v)),
////                                   5 seconds, 1 second)
//    JsCmds._Noop
//  }
  
  def onEmailEnableChange(v: String): JsCmd = {
    log.debug("email updated: " + v)
    v match {
      case "0" => disableEmails
      case "1" => if (hasEmails) enableEmails else { createEmails; repaint; }
    }
    v match {
      case "0" => JqJsCmds.Hide("mail2", new net.liftweb.util.Helpers.TimeSpan(2000))
      case "1" => JqJsCmds.Show("mail2", new net.liftweb.util.Helpers.TimeSpan(2000))
    }
    // that has not effect here. Solution: missing '=' in method declaration
    //JsCmds.Alert("hello world")
  }
  
  def repaint = {
    log.debug("redirecting to parent page")
    //S.redirectTo(parentPage)
    JsCmds.RedirectTo(parentPage)
  }
      
  // ############## the radio button example ends #############

  protected def licenseTypeOption = //("VEI", "VEI") :: ("VSPMOB", "VSPMOB") :: Nil
    licenseTypeOptions.map(i => (i.toString, i.toString))

  def createLicenseType(ltype: String) = SHtml.select(licenseTypeOption,
        Full(ltype),
        v => onChange(v, "licensetype"))
  
  protected def licenseGroupOption = ("P", "Portal") :: ("V", "Vista") :: Nil

  def createLicenseGroup(lgroup: String) = SHtml.select(licenseGroupOption,
        Full(lgroup),
        v => onChange(v, "licensegroup"))
  
  // ############ EDIT ###########
        
  /** toShowProduct - for display: get the selected Product */
  protected def toShowProd = {
    // take the value 'val' from the request 
    // or use the value '1'
    // or take the first row in the table
    val id = S.param("val").map(String.valueOf(_)) openOr productid
    log.debug("selected product id: " + id)
    productid = id
    Pproduct.findForParam(id) openOr Pproduct.findForList(0, 1)(0)
  }
     
  protected def toShowLic(productid: String) = {
    val sql = "select * from plicensegroup where productid = " + productid
    log.debug("sql: " + sql)
    Plicensegroup.findAllByInsecureSql(sql,
        IHaveValidatedThisSQL("frank", "2008-12-03"))(0)
  }
  
  protected def getLicenseTypes = {
    val sql = "select distinct licensetype from plicensegroup"
    Plicensegroup.findAllByInsecureSql(sql,
        IHaveValidatedThisSQL("frank", "2008-12-03"))
  }
     
  protected def hasEmails = {
    val id = S.param("val").map(String.valueOf(_)) openOr productid
    log.debug("selected product id: " + id)
    // double check that we have really an ID of '1'
    val reqid = S.param("val").map(String.valueOf(_)) openOr "-1"
    if (id.equals("1") && reqid.equals("-1"))
      throw new IllegalArgumentException("ID is missing");

    val params: List[String] = List(id)

    val sql = "select count(*) from PMAIL where productlineid = ?" 
    log.debug("sql: " + sql + " " + params)
      
    val res = DB.runQuery(sql, params, portalDB)
    log.debug("res count: " + res)
    
    log.debug("############### Has emails? " + (!res._2(0)(0).equals("0")))
    // runQuery returns a tuple of head and body.
    // we access the body '_2' the first row '0' the first column '0'
    !res._2(0)(0).equals("0")
  }
  
  protected def hasEmailsEnabled = ProductVar.is.productlineid != 9999L 

  protected def disableEmails = {
    val id = productid
    log.debug("selected product id: " + id)
    
    val params: List[String] = List("9999", id)

    val sql = "update pproduct set productlineid = ? where productid = ?" 
    log.debug("sql: " + sql + " " + params)
      
    DB.runUpdate(sql, params, portalDB)
  }
  
  protected def enableEmails = {
    val id = productid
    log.debug("selected product id: " + id)
    
    val params: List[String] = List(id, id)

    val sql = "update pproduct set productlineid = ? where productid = ?" 
    log.debug("sql: " + sql + " " + params)
      
    DB.runUpdate(sql, params, portalDB)
  }
  
  protected def createEmails = {
    val id = productid
    log.debug("selected product id: " + id)
    if (id.equals("4"))
      throw new IllegalArgumentException("The id is '4'. There is probably a program error");
    
    val params: List[String] = List(id)

    val sqlList = List(
        "insert into pmail (productlineid, mailtype, subject, body) " +
        "( select ?,pm.mailtype,pm.subject,pm.body from pmail pm where pm.productlineid=4 and pm.mailtype='N')",
        "insert into pmail (productlineid, mailtype, subject, body) " + 
        "( select ?,pm.mailtype,pm.subject,pm.body from pmail pm where pm.productlineid=4 and pm.mailtype='C')",
        "insert into pmail (productlineid, mailtype, subject, body) " + 
        "( select ?,pm.mailtype,pm.subject,pm.body from pmail pm where pm.productlineid=4 and pm.mailtype='S')"
    )
      
    sqlList.map(sql => DB.runUpdate(sql, params, portalDB))
  }
  

  def update = {
      val name = S.param("name").map(String.valueOf(_)) openOr "no name"
      val id = get("productid")
      val producttype = if (get("producttype").equals("0")) ProductVar.is.producttype.get else get("producttype")
      
      // THIS updates ALL 'description' of contents 'Amadeus Policy Arranger' with 'Amadeus Policy Arranger xxx'
      // and that was not what I expected it to do :-(
//      val product: Pproduct = ProductVar.is
//      product.name.set(name)
//      product.stadiid.set(name)
//      product.application.set(name)
//      product.description.set(get("description"))
//      product.producttype.set(get("producttype"))
//      //product.productid.set(get("productid").toLong)
//      val valiErr = product.validate
//      log.debug("validated: " + valiErr)
//      product.save
      
      {
          val params: List[String] = List(name, name, name, get("description"), producttype, get("productlineid"), id)
          val sql = "update pproduct set name = ?" + 
          ", stadiid = ?"       +
          ", application = ?"   + 
          ", description = ?"   + 
          ", producttype = ?"   +
          ", productlineid = ?" + 
          " where productid = ?"
          log.debug("sql: " + sql + " " + params)
          DB.runUpdate(sql, params, portalDB)
      }
      
      // stadiproduct
      
      // INSERT INTO PSTADIPRODUCT (PRODUCTID, STADIID, DESCRIPTION, SHOWPLANNED)
      // VALUES (##id##, '##name##', '##desc##', 'Y');
      {
          val params: List[String] = List(name, get("description"), "Y", id)
          val sql = "update pstadiproduct set " + 
          "  stadiid = ?"       +
          ", description = ?"   + 
          ", showplanned = ?"   +
          " where productid = ?"
          log.debug("sql: " + sql + " " + params)
          DB.runUpdate(sql, params, portalDB)
      }
      // licensegroup
      
      // INSERT INTO plicensegroup(groupid,productid,licensetype)
      // VALUES ('V',##id##,'VEI');
      {
          val params: List[String] = List(get("licensegroup"), get("licensetype"), id)
          val sql = "update plicensegroup set" + 
          " groupid = ?"       +
          ", licensetype = ?"   + 
          " where productid = ?"
          log.debug("sql: " + sql + " " + params)
          DB.runUpdate(sql, params, portalDB)
      }
  }
  
  
  def get(tag: String) =  {
    var res = S.param(tag).map(String.valueOf(_)) openOr  ""
    // TEST
    log.debug("session data: " + S.get(tag))
    if (res.isEmpty) res = S.get(tag) openOr ""
    log.debug("tag: " + tag + " = " + res)
    res
  }
  
  def sendUpdatesPerEmail(productid: String, name: String, desc: String, producttype: String, productlineid: String) = {
    var text = """
-- newArticle##name##.sql
-- ##name##, ##id##
-- ##desc##

SELECT MAX(PRODUCTID) FROM PPRODUCT;
SELECT MAX(PRODUCTID) FROM PSTADIPRODUCT;
SELECT COUNT(*) FROM PPRODUCT WHERE NAME='##name##';

select * from pproduct where productid in (##id##);
select * from PSTADIPRODUCT where productid in (##id##);
select * from plicensegroup where productid in (##id##);

INSERT INTO PPRODUCT (PRODUCTID, NAME, STADIID, APPLICATION, DESCRIPTION, PRODUCTTYPE, PRODUCTLINEID)
VALUES (##id##, '##name##', '##name##', '##name##', '##desc##', '##type##', ##productlineid## );

INSERT INTO PSTADIPRODUCT (PRODUCTID, STADIID, DESCRIPTION, SHOWPLANNED)
VALUES (##id##, '##name##', '##desc##', 'Y');

INSERT INTO plicensegroup(groupid,productid,licensetype)
VALUES ('V',##id##,'VEI');

-- enable mails
INSERT INTO PMAILCOUNTRY (COUNTRYCODE ,PRODUCTLINEID ,DESCRIPTION ) VALUES ('D' ,##id## ,'Germany'  );

insert into pmail (productlineid, mailtype, subject, body)
( select ##id##,pm.mailtype,pm.subject,pm.body from pmail pm where pm.productlineid=4 and pm.mailtype='N');
insert into pmail (productlineid, mailtype, subject, body)
( select ##id##,pm.mailtype,pm.subject,pm.body from pmail pm where pm.productlineid=4 and pm.mailtype='C');
insert into pmail (productlineid, mailtype, subject, body)
( select ##id##,pm.mailtype,pm.subject,pm.body from pmail pm where pm.productlineid=4 and pm.mailtype='S');

select * from pmail where productlineid=##id##;
select * from pmailcountry where productlineid=##id##;

--disable mails
select * from pmail where productlineid=##id##;
SELECT * FROM PPRODUCT WHERE PRODUCTID=##id## AND NAME='##name##' AND PRODUCTLINEID=##id##;
UPDATE PPRODUCT SET PRODUCTLINEID=9999 WHERE PRODUCTID=##id## AND NAME='##name##' AND PRODUCTLINEID=##id##;
SELECT * FROM PPRODUCT WHERE PRODUCTID=##id## AND NAME='##name##' AND PRODUCTLINEID=##id##;

-- re-enable mails
UPDATE PPRODUCT SET PRODUCTLINEID=##id## WHERE PRODUCTID=##id## AND NAME='##name##' AND PRODUCTLINEID=9999;
      """
      text = text.replace("##name##", name).replace("##id##", productid).
             replace("##desc##", desc).replace("##type##", producttype)
      log.debug ("text: " + text)
      
//    val myRecips : List[String] = List("Frank.Hofmann.Usingen@t-online.de")
//    val plainContent : MailTypes = new PlainMailBodyType(text)
//    Mailer.sendMail(From("no-reply@de.extranet.com"), Subject("Article update message for " + name),
//                    (plainContent :: myRecips.map(To(_))) : _*)
      
      //PortalAdapter.sendMail()
  }
  
}


// Demo
//class HelloWorldxxx {
//  var sex = "M"
//  val sex_map = Map("Male"->"M", "Female"->"F")
//  val radios = SHtml.radio(sex_map.keys.toList, Full(sex), sex = _)
//  
//  def example(xhtml: NodeSeq): NodeSeq = bind("entry", xhtml,
//    "male" -> radios(0),
//    "female" -> radios(1)
//  )
//  
//}
