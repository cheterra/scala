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
object Grammar {

  def showSubject(html: NodeSeq) : NodeSeq = {
      var store = StoreVar.getStore

      bind("item", html,
           "topic"  -> store.category,
           "head"   -> store.chapter
      )
  }
}
