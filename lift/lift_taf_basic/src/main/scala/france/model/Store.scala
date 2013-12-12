package france
package model

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import Helpers._

import net.liftweb._

import mapper._
import util._
import http._

object StoreVar extends SessionVar[Store](new Store) {
  // not really required. You could also use
  //   var store = StoreVar.is
  // instead of
  //   var store = StoreVar.getStore
  def getStore = StoreVar.is
}

/** This is meant to hold data (Quick and dirty and only for one user) */
class Store {
  var usersResponse = "?"

  /* Data */
  var category = "Grammair"
  var chapter =  "Imparfait"
  var index = 0
  //var cardsA = List ("Je (être) fatigué", "Tu (avoir) de la chance")
  //var cardsB = List ("J'étais fatigué",   "Tu avais de la chance")
  var cards: IndexedSeq[Card] = Vector[Card]()
  def setCards(theCards: Seq[Card]) = cards = theCards.toIndexedSeq; reset;

  def check: Unit = {
      if (!hasNext)
        throw new java.lang.IndexOutOfBoundsException("next called (" + index + ") max: " + cards.size)
  }
  def next = check; index = index + 1

  def hasNext = (index + 1) < cards.size
  def reset = index = 0
  def cardA = cards(index).sideA
  def cardB = cards(index).sideB
  def checkResponse = usersResponse.equals(cardB)
}
