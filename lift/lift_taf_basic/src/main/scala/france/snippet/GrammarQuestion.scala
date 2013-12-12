package france
package snippet

import net.liftweb._
import http._
import util.Helpers._
import scala.xml.NodeSeq

import model._

/**
 * A snippet that binds behavior, functions,
 * to HTML elements
 */
object GrammarQuestion {
//  var solution = "quoi?"
//  private object name extends RequestVar("Frank")
//  private object solution extends RequestVar("quoi?")
  var dummy = ""

  // TODO this must be move to an 'init' class or 'select lesson' class
  // no more used. moved to GrammarControl
  def load(in: NodeSeq): NodeSeq = {
    var store = StoreVar.getStore
    // TEST
    try {
      var cread = new CardReader
      cread.load(store)
    } catch {
      case e: Exception => println("Unexp: " + e)
              e.printStackTrace()
    }
    // return dummy
    <div></div>
  }


  def render = {
    var store = StoreVar.getStore

    // process the form
    def process() {
        S.notice("Users response: "+store.usersResponse)
        S.redirectTo("/france/grammar_response")
    }

    if (!store.hasNext) {
      S.error("store has no next value")
      S.redirectTo("/france/index")
    }
    // associate each of the form elements
    // with a function... behavior to perform when the
    // for element is submitted
    // we don't need an explicit function because RequestVar
    // extends Settable{type=String}, so Lift knows how to
    // get/set the RequestVar for text element creation
    //"name=name" #> SHtml.textElem(name) &
    "name=card"     #> SHtml.text(store.cardA, dummy = _) & // set the card
    "name=solution" #> SHtml.text(store.usersResponse, store.usersResponse = _) & // set the solution
    // when the form is submitted, process the variable
    "type=submit"   #> SHtml.onSubmitUnit(process)
  }

}
