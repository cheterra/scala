package cluedo.solver

import cluedo.Player
import cluedo.Card


class Analyser(names: List[String], cards: List[Card]) {
  // 1. if a player has a card optionally in one round but not in an other round
  //   the player has not the card
  // 2. in that case the remaining players of the group are reduced
  // find the groups with same question cards
  val testees: List[Testee] = Testee.makePlayers(names);
  val ctx = new Context(testees, cards)
  val sol: Solutions = new Solutions(ctx);
  Testee.injectSolution(testees, sol);
  
  // this is called for each new round
  def investigate(round: Round) = {
    markPlayerHasNot(round);
    markPlayerMayHave(round);
    // if the player is in more than one group which may have this card
    //   we may identify this player as the owner of the card
    //   1. the other players must be re-calculated
    //  Example: cards A B C   and C D E (C is common and each time player 1 shows a card)
    //combine(round);
    
    // if the player is in a group which may have this card
    //   and the player has not this card
    //   1. the player must be removed from that group
    //   2. the other players in the group must be re-calculated
    // this is done in the player class, when a notInList was done
    
    // if the player is the only one who may have the card,
    //   he has the card.
    //   1. all other players must be marked not to have this card
    
  }

  /**
   * markPlayerHasNot the card(s). Mark store the round completely.
   */
  def markPlayerHasNot(round: Round) = {

    val players: List[Player] = round.nCards match {
	      case 0 => 
	        // if the number of cards was 0 in that round then none 
	        //    of the other players has the card (apart the leader)
		      println ("other: " + ctx.other(List(round.leader)))
		      List(round.leader)
	      case 3 =>
		      // he players showing the card may have the cards
		      //   but the leader cannot have the cards
		      println ("the leader " + round.leader + " cannot have the cards")
		      round.shower
	      case _ =>
		      // the players showing the card and the leader may have the
		      //   card but all players who did not show a card have not the card.
		      //println ("shower: " + round.shower )
		      round.leader :: round.shower;
	    }
      //  other players                has not      cards
      ctx.other(players).foreach(_.markHasNot(round.cards))
  }

  /**
   * markPlayerMayHave -  three, two or one player may have all three cards. 
   * Mark for each player (including leader) that he (may) have this card(s)
   *    but not if the player already 'has not' this card
   */
  def markPlayerMayHave(round: Round) = {
    val players: List[Player] = round.nCards match {
      // the leader and the talon may have the card but none of the players
      case 0 =>     List(round.leader);
      // three players may have the card but not the leader
      case 3 =>     round.shower;
      // two players and the leader and the talon may have the card
      case _ =>     round.leader :: round.shower;
    }
    ctx.asTestee(players).foreach {_.markMaybeLists(round)};
  }
  
  // this is called for each new round
  def combine(round: Round, rounds: List[Round]) {
    // if the player is in more than one group which may have this card
    //   we may identify this player as the owner of the card
    //  Example: cards A B C   and C D E (C is common and each time player 1 shows a card)

      /*
       * Note: This does not work if Gisela shows card A for one round and card B for another round: 
found just one player: Gisela
  and this player was the only one in both rounds:
1. List(Draco Malfoy (who), Raum der Wünsche (where), Unsichtbarkeitsschrank (what))
2. List(Peter Pettigrew (who), Zaubertränke (where), Unsichtbarkeitsschrank (what))
SolutionAdd: have shower: 2, cards shown: 2, cards left: 3, rem: 0, add: 0
SolutionAdd: player: Gisela (List(---Eulerei (where), ---Impedimenta (what), ---Zaubertränke (where), ---Raum der Wünsche (where)))
  has got card: Unsichtbarkeitsschrank (what) 
  
  Here 'Unsichtbarkeitsschrank was common but Gisela has shown
  - Raum der Wünsche
  - Zaubertränke     
  
  Thus this rule is only valid for 3 answers in both rounds
      */
    
    // players 1 - 3
    val players = round.shower;
    val cards = round.cards;
    rounds.foreach (r => {
      val commonCards: List[Card] = r.cards.intersect(cards);
      // just one card?  A B  - C - D E
      // only valid for 3 shown cards
      if (round.nCards == 3 && r.nCards == 3 && commonCards.length == 1) {
        val card = commonCards.head;
        println ("found just one card: " + card);
        // search common player
        val commonPlayers = r.shower.intersect(players)
        if (commonPlayers.length == 1) {
          val player = commonPlayers.head;
          println("found just one player: " + commonPlayers.head.name);
          println("  and this player was the only one in both rounds:\n1. " + round.cards + "\n2. " + r.cards);
          // this player has the card
          val addee = new Addee(player, card);
          sol.solve(addee, round);
          // thus this player + card must be removed from both rounds
          round.remove(player, card);
          // thus this player + card must be removed from both rounds
          r.remove(player, card);
          // all other players and the leader do not have the card
        }
      }
    });
  }
  
  /**
  * In the talon there are three cards, one of each type.
  * If we have confirmed one card of type A in the talon
  * each player which is the only owner of type A must have this card
  */
  def analyseTalon(round: Round) {
    val analysers: List[AnalyseTalonCard] = cards.map(new AnalyseTalonCard(testees, _, sol));
    // talon cards = card with 0 players
    val talonCards = analysers.filter(_.isTalonCard).map(_.card)
    talonCards.foreach(talon => {
      // if the category matches then assign the card to the owner
      analysers.foreach(_.assign(round, talon.cat))
    });
  }

  def showCard(card: Card): List[String] = testees.map { _.showCard(card) }
  
  override def toString() = "  testees: " + testees
}

private class AnalyseTalonCard(testees: List[Testee], val card: Card, val sol: Solutions) {
  
  /** testees which are not the owner of this card */
  val notOwners: List[Testee] =  testees.filter(_.notOwnedCards.contains(card))
  /** card with 0 players must be in talon */
  lazy val isTalonCard: Boolean = notOwners.length == testees.length
  /** card with one possible owner, example: 5 players, 4 are not the owner */
  lazy val isCardOnePlayer: Boolean = notOwners.length == testees.length - 1
  /** Possible owners of the card - subtract all players which are not owners */ 
  lazy val possiblyOwner: List[Testee] =  testees.diff(notOwners)  
  
  // now check the category and assign all one-cards with that category to the owner
  def assign(round: Round, talonCat: String): Unit = {
    // possibly owner = 1
    if (!isCardOnePlayer)
      return;
    // category must match
    if (!card.cat.equals(talonCat))
      return;
    // TEST
    //println("possibly owner length: " + possiblyOwner.length)
    // print only if the player does not have the card yet, else it will be boring
    if (! possiblyOwner.head.cards.contains(card))
      println("since the card of category '" + talonCat + "' is in the talon" +
        " a player with only one card of that type" +
        "\n  must be the owner of that card (" + card + "): " + possiblyOwner.map(_.name));
    // TODO there is still an error
/*
Heike question: --- List(Bellatrix Lestrange (who), Raum der Wünsche (where), Impedimenta (what))
Anna answer:	 None
Julia answer:	 Some(Impedimenta (what))
Gisela answer:	 Some(Raum der Wünsche (where))
Frank answer:	 Some(Bellatrix Lestrange (who))
the leader Heike (List(---Portschlüssel (what), ---Dolores Umbridge (who), ---Verteidigung gdd Künste (where), ---Draco Malfoy (who))) cannot have the cards
Julia: bad cards: List(Bellatrix Lestrange (who))
SolutionRemove3_3: what to do with 0 removees and 0 addees
two players List(Julia, Gisela) have not the card Bellatrix Lestrange (who) thus the remaining one (Frank) must have the card
SolutionMutualExclusive: round: showers left: 3, cards shown: 3, cards left: 3, rem: 0, add: 0
  shower: List(Frank (List(---Petrificus Totalus (what), ---Pokalzimmer (where), ---Bellatrix Lestrange (who))), Gisela (List(---Grabbe and Goyle (who), ---Raum der Wünsche (where), ---Wahrsagen (where), ---Peter Pettigrew (who))), Julia (List(---Krankenflügel (where), ---Alraune (what), ---Eulerei (where), ---Impedimenta (what)))) cards: List(Bellatrix Lestrange (who), Raum der Wünsche (where), Impedimenta (what))
two players List(Gisela, Frank) have not the card Impedimenta (what) thus the remaining one (Julia) must have the card
SolutionMutualExclusive: round: showers left: 3, cards shown: 3, cards left: 3, rem: 0, add: 0
  shower: List(Frank (List(---Petrificus Totalus (what), ---Pokalzimmer (where), ---Bellatrix Lestrange (who))), Gisela (List(---Grabbe and Goyle (who), ---Raum der Wünsche (where), ---Wahrsagen (where), ---Peter Pettigrew (who))), Julia (List(---Krankenflügel (where), ---Alraune (what), ---Eulerei (where), ---Impedimenta (what)))) cards: List(Bellatrix Lestrange (who), Raum der Wünsche (where), Impedimenta (what))
Gisela: bad cards: List(Bellatrix Lestrange (who), Impedimenta (what))
Bad cards length = 2 is a good thing. Gisela must have the third card
SolutionAdd: player: Gisela  has got card: Raum der Wünsche (where)
Frank: bad cards: List(Raum der Wünsche (where), Impedimenta (what))
Bad cards length = 2 is a good thing. Frank must have the third card
found just one card: Bellatrix Lestrange (who)
found just one card: Impedimenta (what)
found just one player: Julia
  and this player was the only one in both rounds:
1. List(Bellatrix Lestrange (who), Raum der Wünsche (where), Impedimenta (what))
2. List(Dolores Umbridge (who), Große Halle (where), Impedimenta (what))
since the card of category 'what' is in the talon a player with only one card of that type must be the owner of that card
SolutionAdd2_2
SolutionAdd: player: Gisela  has got card: Bellatrix Lestrange (who)
since the card of category 'what' is in the talon a player with only one card of that type must be the owner of that card
SolutionAdd2_2
since the card of category 'what' is in the talon a player with only one card of that type must be the owner of that card
SolutionAdd2_2
since the card of category 'what' is in the talon a player with only one card of that type must be the owner of that card
SolutionAdd2_2
 ... Anna ... Julia ... Heike ... Gisela ... Frank
Bellatrix Lestrange       : --- List(-, -, -, X, X)
Peter Pettigrew           : --- List(2, ?, -, ?, -)
Draco Malfoy              : --- List(-, -, X, -, -)
Lucius Malfoy             : --- List(?, 1, -, -, -)
Dolores Umbridge          : --- List(-, -, X, -, -)
Grabbe and Goyle          : --- List(-, -, -, X, -)
Große Halle               : --- List(X, -, -, -, -)
Krankenflügel             : --- List(?, ?, -, -, ?)
Raum der Wünsche          : --- List(-, -, -, X, -)
Zaubertränke              : --- List(X, -, -, -, -)
Pokalzimmer               : --- List(-, ?, -, 2, ?)
Wahrsagen                 : --- List(-, -, -, ?, -)
Eulerei                   : --- List(-, 1, -, -, -)
Bibliothek                : --- List(-, -, ?, -, -)
Verteidigung gdd Künste   : --- List(-, -, X, -, -)
Petrificus Totalus        : --- List(-, -, -, -, X)
Schlafmittel              : --- List(X, -, -, -, -)
Unsichtbarkeitsschrank    : --- List(-, -, -, -, -)
Portschlüssel             : --- List(-, -, X, -, -)
Alraune                   : --- List(3, ?, -, -, -)
Impedimenta               : --- List(-, X, -, -, -)

 */    
    possiblyOwner.foreach(owner => sol.solve(new Addee(owner, card), round))
  }
  
}

