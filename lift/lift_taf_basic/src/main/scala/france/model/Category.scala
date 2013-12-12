package france
package model

import scala.xml._
import scala.collection.immutable.Vector

trait Cat {
  def getCategories: Seq[Cat]
  def getCategory(name: String) = getCategories.filter(_.name.equals(name)).head
  def name: String = null
  def getCards: Seq[Card] = null
}

class Category (cat: Node) extends Cat {
  lazy val deckList = makeDecks
  // Categories may have sub categories
  lazy val catList: Seq[Category] = makeSubCategories

  override lazy val name = (cat \"@name").text

  /** get sub categories */
  def getCategories = catList
  // Categories may have sub categories
  def makeSubCategories =  cat\"Category" filter(!_.isEmpty) map(new Category(_))

  def getDecks = deckList
  // TODO that is duplicate storage of data. Use recursion instead
  def makeDecks = cat\\"Deck" filter(!_.isEmpty) map(new Deck(_))

  override def getCards: Seq[Card] = deckList flatMap (deck => deck.getCards)

  override def toString =  "Category: " + cat
}
