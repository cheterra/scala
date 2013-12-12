package form
package snippet

import net.liftweb._
import http._
import common._
import util.Helpers._
import scala.xml.NodeSeq
/**
 * A RequestVar-based snippet
 */
object ReqVar {
  // define RequestVar holders for name, age, and whence
  private object name extends RequestVar("")
  private object age extends RequestVar("42")
  private object agbst extends RequestVar("9426002")
  private object whence extends RequestVar(S.referer openOr "/")

  def render = {
    // capture the whence... which forces evaluation of
    // the whence RequestVar unless it's already been set
    val w = whence.is

    // we don't need an explicit function because RequestVar
    // extends Settable{type=String}, so Lift knows how to
    // get/set the RequestVar for text element creation
    "name=name" #> SHtml.textElem(name) &
    // test ag/bst
    "name=agbst" #> SHtml.textElem(agbst) &
    // add a hidden field that sets whence so we
    // know where to go
    "name=age" #> (SHtml.textElem(age) ++ 
                   SHtml.hidden(() => whence.set(w))) &
    "type=submit" #> SHtml.onSubmitUnit(process)
  }

  // process the same way as
  // in Stateful
  private def process() =
    asInt(age.is) match {
      case Full(a) if a < 13 => S.error("Too young!")
      case Full(a) => {
        S.notice("Name: "+name)
        S.notice("Age: "+a)
        S.notice("AG/BST: "+agbst)
        S.redirectTo(whence)
      }
      
      case _ => S.error("Age doesn't parse as a number")
    }
}
