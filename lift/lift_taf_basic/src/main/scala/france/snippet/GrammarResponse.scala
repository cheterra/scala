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
object GrammarResponse {
  var dummy = ""
  var store = StoreVar.getStore

  def render = {
    // define some variables to put our values into

    // process the form
    def process() {

      if (!store.checkResponse)  {
        S.error("Wrong solution!")
        S.redirectTo("/france/grammar_question")
      }
      else {
        // otherwise give the user feedback and
        // redirect to the next question page
        S.notice("Users response (notice): "+store.usersResponse)
          S.notice("Correct solution");
        if (store.hasNext)  {
          store.next
          S.redirectTo("/france/grammar_question")
        }
        else {
          store.reset
          S.redirectTo("/france/grammar_finished")
        }
      }
    }

    // associate each of the form elements
    // with a function... behavior to perform when the
    // for element is submitted
    //"name=name"     #> SHtml.onSubmit(name = _)     & // set the name
    // we don't need an explicit function because RequestVar
    // extends Settable{type=String}, so Lift knows how to
    // get/set the RequestVar for text element creation
    //"name=solution" #> SHtml.text(solution, solution = _) & // set the solution
    "name=card"     #> SHtml.text(store.cardA, dummy = _) & // set the card
    "name=solution" #> SHtml.text(store.usersResponse, dummy = _) &
    "name=users"    #> SHtml.text(store.usersResponse, dummy = _) &
    "name=response" #> SHtml.text(store.cardB, dummy = _) &
    // when the form is submitted, process the variable
    "type=submit"   #> SHtml.onSubmitUnit(process)
  }

  def getUsersResponse = store.usersResponse

  def response(html: NodeSeq) : NodeSeq = {
      var store = StoreVar.getStore
      var resultOK = <span>The result was <span class='correct'>{getUsersResponse}</span> OK !</span>
      var resultKO = <span>The result was <span class='wrong'>  {getUsersResponse}</span> KO !</span>
      def getResult : NodeSeq = if (store.checkResponse) resultOK else resultKO
      bind("item", html,
           "result"   ->  getResult
      )
  }

}
