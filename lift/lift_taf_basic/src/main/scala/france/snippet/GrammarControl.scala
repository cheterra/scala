package france
package snippet

import net.liftweb._
import util.Helpers._
import common._
import http._
import sitemap._
import scala.xml.NodeSeq

import model._

object GrammarControl {

  var lesson: Lesson = (new CardReader).lesson

  def makeCatList: Seq[(String, String)] = {
//    val cread = new CardReader
//    lesson = cread.lesson
// TODO: The category 'I.' is not sufficiently specified by its name
// in fact it should be 'All##Le Temps##Lektion 57##I.'
// This would be also more efficient navigating the tree
    lesson.getCategories.flatMap(cat =>
      // the first element is the plain name as text and value. Example: All/All
      Seq((cat.name, cat.name)) ++
      // the second element is the name prefixed with is precedor and as text the name with an indentation:
      //   All##First/  +-- First
      makeSubCatList(cat, " +--", new Path(cat)))
  }

  def makeSubCatList(cat: Category, prefix: String, cpath: Path): Seq[(String, String)] = {
    cat.getCategories.flatMap(sub =>
      // create one element like "All##First/  +-- First" and
      Seq((cpath + sub.name, prefix + " " + sub.name)) ++
      // append the rest of the list recursively
      makeSubCatList(sub, "|  " + prefix, new Path(sub, cpath)))
  }

  def getCards(strCatPath: String) = {
    val path = new PathSolver(strCatPath).solved
    var cat: Cat = lesson //lesson.getCategory(path(0))
    path.foreach(name => cat = cat.getCategory(name))
    cat.getCards
  }

  var selected: String = "nothing"

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
    // process the form
    def process() {
        S.notice("Users")
        S.redirectTo("/france/grammar_question")
    }

    def processSel(sel: String) = {
      var store = StoreVar.getStore
      selected = sel;
      println("selected: " + sel);
      store.setCards(getCards(sel))
    }
    // associate each of the form elements
    // with a function... behavior to perform when the
    // for element is submitted
    // we don't need an explicit function because RequestVar
    // extends Settable{type=String}, so Lift knows how to
    // get/set the RequestVar for text element creation
    //"name=cat"     #> SHtml.select(List(("Cat1", "Cat2")), Empty, selected = _) &
    "name=cat"     #> SHtml.select(makeCatList, Empty, processSel(_)) &
    "type=submit"  #> SHtml.onSubmitUnit(process)
  }


}
