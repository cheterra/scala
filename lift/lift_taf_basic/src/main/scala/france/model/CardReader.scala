package france
package model

import scala.xml._
import scala.collection.immutable.Vector
import net.liftweb.util.Props


/** Card reader - reads XML files with vocabulary or grammar into memory */
class CardReader {
  lazy val lesson = load

  def filename = Props.get("france.test.file", "/windows/data/work/ProgSprachen/lift/lift_taf_basic/test.xml")
  
  def load = {
    var root: Elem = XML.loadFile(filename);
    println("root2: " + root)
    new Lesson(root)
  }

  def load(store: Store) = {
    var root: Elem = XML.loadFile(filename);
    println("root: " + root)
    root\"Category" foreach{ (entry) =>
      var cat = new Category(entry)
      store.setCards(cat.getCards)
      // TEST
      //cat.getCards foreach(card => println("The cards: \n" + card))
    }
  }

  def doTest(entry: Elem) {
      var layer = (entry\"Deck").head
      //results = results :+ layer
      var deck = new Deck(layer)
      println("The deck: " + deck);
      //store.setCards(deck.getCards)
      // TEST
      //println("\n entry: \n" + entry + ",\n results: \n" + results)
      var c = new Card((layer\"Card").head)
      println("The card: " + c.toParam)
  }
// save
//   final  def    save(filename: String, node: Node, enc: String = encoding, xmlDecl: Boolean = false, doctype: DocType = null): Unit   
}
