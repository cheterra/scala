package france
package model

import scala.xml._
import scala.collection.immutable.Vector


class Deck (deck: Node) {
  lazy val cardList = makeCards
  def makeCards = deck\"Card" filter(!_.isEmpty) map(new Card(_))
  def getCards = cardList
//  {
//    deck\"Card" foreach{ card =>
//      if (!card.isEmpty) cardList = cardList :+ new Card(card)
//    }
//    cardList
//  }
  override def toString =  "Deck: " + deck
}
