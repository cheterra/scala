package cluedo.solver

import cluedo.Player
import cluedo.Card

object Testee {
  /** for each name make one Player */ 
  def makePlayers(names: List[String]) = names.map( { new Testee(_)} );
  def injectSolution(testees: List[Testee], sol: Solutions) = testees.foreach(_.sol = sol);
}

class Testee(name: String) extends Player(name: String) {
  // @inject Solution
  var sol: Solutions = null;
  
  var notOwnedCards: List[Card] = List()
  /** List with two or three cards and the player owns one of them */
  var ownedMaybeList: List[Round] = List()
  // mark that this cards are not in the players possession
  def markHasNot(cards: List[Card]) = {
    // if that card is already marked then give up
    val toBeRemoved = cards.diff(notOwnedCards);
    // mark has not
    notOwnedCards = toBeRemoved ::: notOwnedCards;
    // and remove those cards from the maybe list
    toBeRemoved.foreach(removeMaybe(_))
  }
  // add cards (rounds) to maybe list but not if the card is already on the 'has not' list
  def markMaybeLists(round: Round) = {
    // is it already clear that this player does not own the card?
    val badCards: List[Card] = round.cards.intersect(notOwnedCards)
    if (!badCards.isEmpty) {
      println (name +": bad cards: " + badCards)
      // try what says the remove solution
      val testee: Testee = this;
      // mark if this is the leader and he has none of the cards
      if (badCards.length == 3 && this.equals(round.leader)) {
        println(name + " is the leader and has none of the cards");
        round.leaderHasNone = true;
        // if there are two players left and two cards, they own those cards mutually exclusive
        if (round.nCards == 2)
          sol.solve(round)
          //sol.solve(round);
          //new Solution(ownedMaybeList).solve(round);
      }
      val isLeader = this.equals(round.leader);
      // bad cards length = 2 is a good thing. This player must have the third card
      if (badCards.length == 2 && !isLeader) {
        println ("Bad cards length = 2 is a good thing. " + name + " must have the third card")
        //Solution(new Addee(testee, round.cards.diff(badCards).head), ownedMaybeList).solve(round)
        sol.solve(new Addee(testee, round.cards.diff(badCards).head), round)
      } 
      else
        badCards.foreach(card => { sol.solve(new Removee(testee, card), round) })
        //badCards.foreach(card => { Solution(new Removee(testee, card), ownedMaybeList).solve(round) })
    }
    if (badCards.isEmpty)
      ownedMaybeList = round :: ownedMaybeList;
  }
  
  def showCard(card: Card): String =     if (cards.contains(card)) "X" else showNotCard(card)
  def showNotCard(card: Card): String =  if (notOwnedCards.contains(card)) "-" else showMayCard(card)
  /** Show the number of players in a (first) group which have shown this card */
  def showMayCard(card: Card): String =  {
    val head: Option[List[Player]] = getMaybeCardGroups(card).headOption;
    head match {
      case Some(group) => group.length.toString
      case None =>  "?"
    }
  }
  /** get the list of groups which may have this card */
  def getMaybeCardGroups(card: Card): List[List[Player]] = 
    ownedMaybeList.map {_.getPlayersFor(card)}.filter(!_.isEmpty)
    
  /** if the testee has not a card the card/player/group must be removed from the maybe list */
  private def removeMaybe(card: Card) = {
    // if the group in maybe list contains the card the group should be removed from the list
    // er, no. Not really. In fact only this one card should be removed
    val removee: Removee = new Removee(this, card);
    //Solution(removee, ownedMaybeList.filter(_.cards.contains(card))).solve;
    val rounds = ownedMaybeList.filter(_.cards.contains(card));
    rounds.foreach( round => {
      round.addRemovee(removee); 
      sol.solve(removee, round) 
    })
  }
  
  override def toString() = super.toString() + "\n  not owned cards: " + notOwnedCards.distinct + "\n";
}

