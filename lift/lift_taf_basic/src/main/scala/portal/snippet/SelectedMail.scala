package portal
package snippet 

import scala.xml.{NodeSeq, Text}
import scala.collection.mutable.{ListBuffer}
import net.liftweb.util._

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


class SelectedMail {
  val log = Logger("portal.snippets.SelectedMail")
  
  // that RequestVar stuff makes me crazy!
  // It is set to default with each submit. The opposite I want!
  //object CurrentMailSet extends RequestVar[List[Pmail]](toMail)
  //object CurrentMail    extends RequestVar[Pmail](CurrentMailSet.is(0))
  //var mail: Pmail = CurrentMailSet.is(0)
  lazy val mailset = toMail
  var mail: Pmail = if (mailset.isEmpty) null else mailset(0)
  
     // TODO: use options N, C, S instead of 0, 1, 2
     protected def emailTypeOption = ("0") :: ("1") :: ("2") :: Nil
     
     def mailTabs(html: NodeSeq) : NodeSeq = {
         log.debug("this: " + this)
         log.debug("--------------------------- new cycle -------------------------")
         // if this is a product without email then display nothing
         if (mailset.isEmpty)
           return <div>No mail - in case you have pressed 'enable' please reload that page</div>
         // if not all mailtypes define do nothing
         if (mailset.size < 3)
           return <div>Not all mails defined</div>   
           
         bind("item", html,
              FuncAttrBindParam("subject", {html: NodeSeq =>
                 Text(mail.subject.toString)},"value"),
              "productlineid" -> mail.productlineid,
              "mailexp"  -> mailexpAsText,
              //"mailtype" -> mail.mailtype,
              "tab1"  -> doTab(mailset(0)) _,
              "tab2"  -> doTab(mailset(1)) _,
              "tab3"  -> doTab(mailset(2)) _,
              "subject"-> subjectInput(mail.subject.get),
              "body"   -> bodyArea(mail.body.get),
              "trans"  -> transArea(translate2(mail.body.get)),
              "submit" -> SHtml.submit("Save email", save _)
         )
     }

  // TODO wouldn't it be better to give up all that html dynamic
  //      and to renew completely the email panel?
  
  def doTab(mailx: Pmail)(text: NodeSeq) =
    SHtml.a(() => {
      log.debug("this: " + this)

  	  log.debug("############ doTab: the old mailtype is: " + mail.mailtype  + " ###################")
  	  mail = mailx

      JsCmds.SetHtml("mailexp5",   <span>{mailexpAsText}</span>) &
      JsCmds.SetHtml("mailtype5",  mailtypeHidden(mailx.mailtype.get)) &
      JsCmds.SetHtml("subject5",   subjectInput(mailx.subject.get)) &
      JsCmds.SetHtml("body5",      bodyArea(mailx.body.get)) &
      JsCmds.SetHtml("trans5",     transArea(translate2(mailx.body.get)))} , 
      text,
      SHtml.ElemAttr.pairToBasic("class", "not-selected"))
  
     def mailtypeHidden(text: String)  = {
       // does not work?
       SHtml.hidden(v => onTypeChange(v, "mailtype"), text)
     }
  
     def subjectInput(text: String)  = {
       log.debug("############ changing subject: the mailtype is: " + mail.mailtype  + " ###################")
       SHtml.ajaxText(text, false, {v:String => onSubjectChange(v, "subject")},
           SHtml.ElemAttr.pairToBasic("maxlength", "100"),
           SHtml.ElemAttr.pairToBasic("size", "80"))
     }
  
     def bodyArea(text: String)  = {
       SHtml.ajaxTextarea(text, {v:String => onTextChange(v, "body")},
           SHtml.ElemAttr.pairToBasic("rows", "10"))
     }

     def transArea(text: String)  = {
       //(( def textarea (value: String, func: (String) => Any, attrs: ElemAttr*)))
       SHtml.textarea(text, v => onChange(v, "trans"),
           SHtml.ElemAttr.pairToBasic("rows", "10"))
     }

     /** here the edited mail body will be saved into the current mail.body variable */
	 protected def onTextChange(v: String, name: String) = {
    	 log.debug(name + " updated")
    	 mail.body.set(v)
    	 log.debug("############ changing body: the mailtype is: " + mail.mailtype  + " ###################")
     }

	 protected def onSubjectChange(v: String, name: String) = {
   	   log.debug(name + " updated")
   	   mail.subject.set(v)
	   JsCmds._Noop
	 }
	 
	 val mailexp = Map("N" -> "New", "C" -> "Change", "S" -> "Storno")
	 
     def mailexpAsText = (mail.mailtype + " -> " + mailexp(mail.mailtype))  
	 
	 protected def onTypeChange(v: String, name: String) = {
	   // TODO mail = mailset(v)
	   v match {
	     case "N" => mail = mailset(0)
	     case "C" => mail = mailset(1)
	     case "S" => mail = mailset(2)
	   }
  	   log.debug("############ changing type: the mailtype is: " + mail.mailtype  + " ###################")
	 }

     def translate2(text: String): String = {
       if (text == null || text.isEmpty)
         return "no mail defined"
       val t = "  \t"
       text.replace("%r", "\n").replace("%t", t).
                  // Liebe(r)
                  replace("%s0", "").
                  // Frau Müller
                  replace("%s1", "Frau Müller").
                  // AG/BST
                  replace("%s2", "001234/001").
                  // Termid \t Produkt
                  replace("%s3", "654321" + t + "Kekse")
     }

	 protected def onChange(v: String, name: String) = {
    	 log.debug(name + " updated: " + v)
    	 S.set(name, v)
    	 //data += name -> v
    	 // TEST
    	 //log.debug("filling map: " + data)
     }

     protected def toShow = {
      // take the value 'val' from the request 
      // or use the value '1'
      // or take the first row in the table
      val id = S.param("val").map(String.valueOf(_)) openOr "1"
      log.debug("selected product id: " + id)
      Pproduct.findForParam(id) openOr Pproduct.findForList(0, 1)(0)
     }
     
    private def toMail = {
      log.debug("###!!!!!!!!!!!!!!!### selecting new data")
      val product = toShow
      val sql = "select * from PMAIL where productlineid = " + product.productlineid
      log.debug("sql: " + sql)
      // TEST (workaround for empty field 'body')
      // unfortunately reading the mail with Pmail.find
      // the body field is always empty
      // Therefore we must read the body conventionally and
      // fill the empty field body of Pmail
      val res: (List[String], List[List[String]]) = DB.runQuery(sql, Nil, portalDB)
      log.debug("res: " + res)
      // res._1 = columns, res._2 = data
      val rows: List[List[String]] = res._2
      // 1: mailtype, 3: body
      val mailMap = rows map { t => (t(1), t(3)) } toMap
      // Bug: Pmail.find has empty 'body'. 
      val mails = Pmail.findAllByInsecureSql(sql,
        IHaveValidatedThisSQL("frank", "2008-12-03"))
      for (i <- 0 until mails.length) {
        // id = 3 is 'body'
//        mails(i).body.set(rows(i)(3))
//        log.debug("text: " + rows(i)(3))
        val mtype = mails(i).mailtype
        mails(i).body.set(mailMap(mtype))
      }
      mails
    }
    
  def save = {
    log.debug("this: " + this)
    log.debug("save")
    val mailtype    = mail.mailtype.toString
    log.debug("############ the mailtype(save) is: " + mailtype  + " ###################")
    update(mailset(0))
    update(mailset(1))
    update(mailset(2))
    "done"
  }
  
  def update(mail: Pmail) = {
      val subject       = mail.subject.get
      val body          = mail.body.toString
      val productlineid = mail.productlineid.toString
      val mailtype      = mail.mailtype.toString
      
      log.debug("############ the mailtype is: " + mailtype  + " ###################")
      // TODO: prevent hacking
      
      val params: List[String] = List(subject, body, productlineid, mailtype)
      val sql2 = "update pmail set subject = ?" + 
      ", body = ?"       +
      " where productlineid = ?" +
      " and mailtype = ?"
      log.debug("sql: " + sql2 + " " + params)
      
      DB.runUpdate(sql2, params, portalDB)
  }
      
  def get(tag: String) =  {
    var res = S.param(tag).map(String.valueOf(_)) openOr  ""
    // TEST
    log.debug("session data: " + S.get(tag))
    if (res.isEmpty) res = S.get(tag) openOr ""
    log.debug("tag: " + tag + " = " + res)
    res
  }
  
}

/*
Sehr geehrte%s0 %s1, %r %rvielen Dank fur Ihre Bestellung. Fur die aufgefuhrten Arbeitsplatze wurden die gewunschten Anderungen umgesetzt. Die neuen Leistungen fur die AG/BST %s2 stehen Ihnen ab sofort wie folgt zur Verfugung: %r %rTerminal%tProdukt %r-------%t----------------------------%r%s3%r%rDie Zahlungspflicht beginnt mit dem Termin der Bereitstellung der Software durch Amadeus Germany. %r %rMit freundlichen Gru?en %r %rAmadeus Germany GmbH
Sehr geehrte%s0 %s1, %r %rvielen Dank für Ihre Bestellung. Für die aufgeführten Arbeitsplätze wurden die gewünschten Änderungen umgesetzt. Die neuen Leistungen für die AG/BST %s2 stehen Ihnen ab sofort wie folgt zur Verfügung: %r %rTerminal%tProdukt %r--------%t----------------------------%r%s3%r%rDie Zahlungspflicht beginnt mit dem Termin der Bereitstellung der Software durch Amadeus Germany. %r %rMit freundlichen Grüßen %r %rAmadeus Germany GmbH
Sehr geehrte%s0 %s1, %r %rvielen Dank für Ihre Bestellung. Die folgenden Lizenzen stehen Ihnen für die AG/BST %s2 ab sofort zur Verfügung: %r %rTerminal%tProdukt %r--------%t----------------------------%r%s3%r%rBitte stellen Sie sicher, dass alle notwendigen Systemvoraussetzungen erfüllt sind. Die Systemvoraussetzungen sowie alle Informationen rund um die Installation finden Sie unter %r%r http://www.de.amadeus.com/reisebuero/installation %r%rDie Zahlungspflicht beginnt mit der Installation der Lizenz/en bzw. spätestens 7 Tage nach Eingang dieser Mail.%r %r %r %rWir wünschen Ihnen viel Erfolg mit Ihrem neuen Produkt! %r %rMit freundlichen Grüßen %r %rAmadeus Germany GmbH
Sehr geehrte%s0 %s1, %r %rvielen Dank für Ihre Bestellung. Die folgenden Lizenzen stehen Ihnen für die AG/BST %s2 ab sofort zur Verfügung: %r %rTerminal%tProdukt %r--------%t----------------------------%r%s3%r%rBitte stellen Sie sicher, dass alle notwendigen Systemvoraussetzungen erfüllt sind. Die Systemvoraussetzungen sowie alle Informationen rund um die Installation finden Sie unter %r%r http://www.de.amadeus.com/reisebuero/installation %r%rDie Zahlungspflicht beginnt mit der Installation der Lizenz/en bzw. spätestens 7 Tage nach Eingang dieser Mail.%r %r %r %rWir wünschen Ihnen viel Erfolg mit Ihrem neuen Produkt! %r %rMit freundlichen Grüßen %r %rAmadeus Germany GmbH
Sehr geehrte%s0 %s1, %r %rwir bedauern Ihre Kündigung sehr und möchten Sie hiermit darüber informieren, dass Ihnen folgende Produktlizenzen fur die AG/BST %s2 ab sofort nicht mehr zur Verfügung stehen: %r %rTerminal%tProdukt %r--------%t----------------------------%r%s3%r %rMit freundlichen Grüßen %r %rAmadeus Germany GmbH
*/