package france
package snippet

import net.liftweb._
import util.Helpers._
import common._
import http._
import sitemap._
import scala.xml.NodeSeq

import model._

// no more used
object GrammarMenu {
  // Create a menu for /france/param
  //val menu = Menu.i("FranceParam") / "france" / "param"

  val menu = Menu.i("FranceParam") / "france" / "param" submenus
  (
    Menu.i("sub1") / "france" / "param1",
    Menu.i("sub2") / "france" / "param2"
  )

  lazy val menu2: List[ConvertableToMenu] = makeMenu

  def makeMenu = {
    val cread = new CardReader
    var lesson = cread.lesson
    // TEST
    //lesson.getCategoryNames.foreach(name => println("#########################################" + name))
    var res = lesson.getCategories.map(cat =>
        (Menu.i("France-" + cat.name) / "france" / "grammar_question_x" / ** >> Loc.Snippet(cat.name, render)
        submenus (makeSubMenu(cat)))).toList
    //println("res: " + res)
    //println("res2: " + lesson.getCategoryNames.map(name => (Menu.i("France-" + name) / "france" / name).name))
    res
  }


  // OK, it seems not to work or I do not get the clue yet
  // So I am going to use the page itself to list the options for one level and so on
  //    somehow like in the recurse sample or like seen in some other apps
  // Nevertheless the '**' parameter will be helpful
  def makeSubMenu(cat: Category) = {
    cat.getCategories.map(sub =>
        Menu.i("FranceX-" + sub.name) / "france" / "grammar_question_y" / ** >> Loc.Snippet(sub.name, render) ).toList
//        Menu.i("France-" + sub.name) / "france" / "grammar_question_x" / (sub.name) >> Loc.Snippet(sub.name, render) ).toList
  }

  def render = "*" #> { " ha, ha"}

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

}
