package cluedo.solver

import cluedo.Card

/** 
 * Covering class of solution which provides the Context for
 * the other solution classes
 */
class Solutions(val ctx: Context) {
  
	def solve(removee: Removee, round: Round): Unit = 
	  new SolutionRemove(removee).solve(round);
	
	def solve(addee: Addee, round: Round): Unit = 
	  new SolutionAdd(addee).solve(round);
  
	def solve(round: Round): Unit = 
	  new SolutionMutualExclusive().solve(round);  


	private class Solution {
	  //def solve: Unit = { rounds.foreach(solve(_)) };
	  def solve(rounds: List[Round]): Unit =  { rounds.foreach(solve(_)) };
	  def solve(round: Round): Unit = {}
	  // shortcut
	  def solve(addee: Addee, round: Round): Unit = new SolutionAdd(addee).solve(round)
	  // TEST
	  // injection
	  //def asTestee(f: Context => List[Testee])
	}
	
	/** Removing a card */	
	private class SolutionRemove(removee: Removee) extends Solution {
	  // if the group in maybe list contains the card the group should be removed from the list
	  // er, no. Not really. In fact only this one card should be removed
	  override def solve (round: Round) = {
	    if (round.shower.length == 3 && round.nCards == 3)  {
	      new SolutionRemove3_3(removee).solve(round); 
	    } else if (round.shower.length == 2 && round.nCards == 3 && round.cards.length == 2) {
	      new SolutionRemove2_2(removee).solve(round); 
	    } else if (round.shower.length == 1                      && round.cards.length == 1) {
	      new SolutionRemove1_1(removee).solve(round); 
	    } else if (round.cards.length < 3)
	      println (" do not know what to do with " + round.shower.length + " players and " + round.nCards + " shown cards and remaining " + round.cards.length + " cards");
	    // try it
	    new SolutionAllCardsRelated(removee).solve(round);
	  }
	
	}
	
	private class SolutionRemove3_3(removee: Removee) extends SolutionRemove(removee) {
	// in that case (3 answers) Frank has none of the cards he has asked for!
	// in that case (neither Heike nor Gisela has the card), Julia must have 'Verteidigung gdd Künste'
	//   and accordingly Heike and Gisela have the other two cards mutually exclusive
	/*   
	Frank question: --- List(Peter Pettigrew (who), Verteidigung gdd Künste (where), Alraune (what))
	Anna answer:	 None
	Julia answer:	 Some(Verteidigung gdd Künste (where))
	Heike answer:	 Some(Peter Pettigrew (who))
	Gisela answer:	 Some(Alraune (what))
	Heike: bad cards: List(Verteidigung gdd Künste (where))
	Gisela: bad cards: List(Verteidigung gdd Künste (where))
	*/
	  override def solve (round: Round) = {
	    if (round.removees.length > 3 || round.addees.length > 1)
	      println ("SolutionRemove3_3: what to do with " + round.removees.length + " removees and " + round.addees.length + " addees");
	    else
	      println ("SolutionRemove3_3");
	    //println("The leader " + round.leader + " cannot have the cards");
	    
	    if (round.removees.length > 2) println("detail: " 
	        + round + " removed: " + round.removees.map(_.player.name));
	    //println("there are two removees");
	    // so which player rests?
	    // example:
	    //   Heike answer:	 Some(Verteidigung gdd Künste (where))
	    //   Gisela answer:	 Some(Peter Pettigrew (who))
	    //   Frank answer:	 Some(Unsichtbarkeitsschrank (what))
	    // and we know: two removees e.g.
	    //   Heike has not 'Peter Pettigrew'
	    //   Frank has not 'Verteidigung ..'
	    // Hmm, then we have still 4 tuples
	    
	    // if two of the showers do not have this card
	    //   the remaining one must have the card
	    val showers = ctx.asTestee(round.shower)
	    round.cards.foreach (card => {
	      val notOwners: List[Testee] = showers.filter(_.notOwnedCards.contains(card));
	      val remainingPlayers = showers.diff(notOwners);
	      if (remainingPlayers.length == 1 && round.nCards == 3) {
	        println("two players " + notOwners.map(_.name) 
	            + " have not the card " + card + " thus the remaining one (" + remainingPlayers.head.name + ") must have the card");
	        remainingPlayers.foreach(p => solve(new Addee(p, card), round))
	        // and accordingly Heike and Gisela have the other two cards mutually exclusive
	        new SolutionMutualExclusive().solve(round);
	      }
	    });
	  }
	
	}
	
	/** 
	 * Solution Remove 2_2
	 * <p> Players: A, B, cards: 1, 2 </p>
	 * if player A owns card 1 the player B owns card 2 and vice versa
	 */
	private class SolutionRemove2_2(removee: Removee) extends SolutionRemove(removee) {
	  // if there are two players and two cards
	  override def solve (round: Round) = {
	    println("SolutionRemove2_2");
	    val cards: List[Card] = round.cards;
	    // Players: A, B, cards: 1, 2
	    // if player A owns card 1 the player B owns card 2 and vice versa
	    //   subtracting the removed card from the two cards left
	    val lastCard: Card = cards.diff(List(removee.card)).head
	    //   subtracting the removed player from the players left
	    val lastPlayer = round.shower.diff(List(removee.player)).head
	    // the last card is owned by the removee
	    solve(new Addee(removee.player, lastCard), round);
	    //println("Solution2_2: player 1: " + removee.player +  " has got card:\n" + lastCard)
	    // and the removed card must be owned by the last player
	    solve(new Addee(lastPlayer, removee.card), round);
	    println("Solution2_2: player 2: " + lastPlayer +  " has got card:\n " + removee.card)
	    // this round is solved, the result is in the players
	  }
	}
	
	// very rare
	/** 
	 * This player was the only one who has shown a card (one of three) 
	 * and now one (of three) has been removed
	 * thus two are left
	 */
	private class SolutionRemove1_1(removee: Removee) extends SolutionRemove(removee) {
	  // if there are two players and two cards
	  override def solve (round: Round) = {
	    println("SolutionRemove1_1");
	    println (" this is a rare case. 1 players and 1 cards: " + round.cards);
	    println("player " + removee.player + " may still have " + round.cards + " but not this one: " + removee.card);
	    
	    // TEST remove one of them (works only for nCards = 3)
	    // if player A has not the card and player B has not the card then player C must have the card
	    if (round.shower.length == 3) {
	      val players = ctx.asTestee(round.shower.diff(List(removee)));
	      val playersMissing = players.filter(_.notOwnedCards.contains(removee.card))
	      val playersHaving = players.diff(playersMissing)
	      // just one? must have the card!
	      if (playersHaving.length == 1) {
	        println("Only player " + playersHaving.head.name + " can have the card " + removee.card 
	            + " since the others have not")
	        playersHaving.foreach(player => solve(new Addee(player, removee.card), round));
	      }
	    }
	  }
	}
	
	/** We have found a player who has a card */
	private class SolutionAdd(addee: Addee) extends Solution {
	  override def solve (round: Round) = {
	    if (round.shower.length == 2 && round.nCards == 3 && round.cards.length == 2) {
	      new SolutionAdd2_2(addee).solve(round);
	    } else {
	      new SolutionAdd1(addee).solve(round);      
	    }
	  }
	}
	
	/** Simply add a card to the players possession. Remove the card from the other players */
	private class SolutionAdd1(addee: Addee) extends Solution {
	  override def solve (round: Round): Unit = {
	    // if the player has already the card we do not need to add it a second time
	    if (addee.player.cards.contains(addee.card))
	      return
	    //println("SolutionAdd: have " +  round );
	    if (round.shower.length == 2 && round.nCards == 3 && round.cards.length == 3)
	      println("SolutionAdd1: we have 2 players and two cards, we can do something: \n  " + round.dump)
	    // we have found a player who has a card
	    ctx.asTestee(addee.player).addCard(addee.card)
	    println("SolutionAdd1: player: " + addee.player.name +  "  has got card: " + addee.card);
	    // since this player has the card, all other players do not have that card
	    val others: List[Testee] = ctx.other(List(addee.player));
	    others.foreach(_.markHasNot(List(addee.card)))
	    // make a note
	    round.addAddee(addee);
	    
	    new SolutionAllCardsKnown(addee).solve(round);
	  }
	}
	
	private class SolutionAllCardsKnown(addee: Addee)  extends Solution {
	  override def solve (round: Round): Unit = {
	    // if we know all cards of that player
	    val player: Testee = ctx.asTestee(addee.player);
	    if (player.allCardsKnown) {
	      println("SolutionAllCardsKnown: We know all cards of " + player.name +
	          ", thus we remove all other cards still pending");
	      val otherCards = ctx.otherCards(player.cards);
	      player.markHasNot(otherCards);
	    }
	  }
	}
	
	private class SolutionAllCardsRelated(removee: Removee)  extends SolutionRemove(removee) {
	  override def solve (round: Round): Unit = {
	    // if only n cards are left which may be only owned by this
	    //   player and we know he has just n cards
	    //   he must own all of those cards
	    val testee: Testee = ctx.asTestee(removee.player);
      val possibleCards: List[Card] = ctx.cards.diff(testee.notOwnedCards);
      val allCardsRelated = possibleCards.length == testee.nCards
      // but skip if he already owns all the cards (would be boring)

	    if (allCardsRelated && !testee.allCardsKnown) {
	      val n = testee.nCards;
	      val pendingCards = testee.cards.diff(possibleCards);
	      println("SolutionAllCardsRelated: Player " + testee.name +
	          " has " + n + " cards\n  and only " + n + " cards are left for assignment to this player,\n  " +	         
	          "thus he has all cards still pending: " + pendingCards);
	      pendingCards.foreach(card => new SolutionAdd1(new Addee(testee, card)).solve(round) )
	    }
	  }
	}
	
	/** Adding two players, two cards */
	private class SolutionAdd2_2(addee: Addee) extends SolutionAdd(addee) {
	  // if there are two players and two cards
	  override def solve (round: Round) = {
	    println("SolutionAdd2_2 " + addee.player.name);
	    // add to the first round and then add remaining player
	    new SolutionAdd1(addee).solve(round);
	    val cards: List[Card] = round.cards;
	    // Players: A, B, cards: 1, 2
	    // if player A owns card 1 the player B owns card 2
	    val lastCard: Card = cards.diff(List(addee.card)).head
	    val lastPlayer = round.shower.diff(List(addee.player)).head
	    // so, the last card is owned by the last player
	    //   and this is the beginning of a new solution
	    new SolutionAdd1(new Addee(lastPlayer, lastCard)).solve(round);
	    // this round is solved, the result is in the players
	  }
	}
	
	// two players, two cards
	private class SolutionMutualExclusive extends Solution {
	  // if there are two players and two cards
	  override def solve (round: Round) = {
	    //println("SolutionMutualExclusive");
	    println("SolutionMutualExclusive: round: " + round + "\n  shower: " + round.shower + " cards: " + round.cards);
	    if (round.isShowersRound && round.shower.length == 2 && round.cards.length == 2) {
	      println("mutually exclusive");
	      // can we exclude player 1 having card 1?
	      val showers: List[Testee] = ctx.asTestee(round.shower);
	      showers.foreach(player => {
		      val missingCards: List[Card] = player.notOwnedCards.intersect(round.cards)
		      if (missingCards.length == 1) {
		        // player 1 must have the other card, player 2 must have this card
		        new SolutionRemove2_2(new Removee(player, missingCards.head)).solve(round);
		      } 
	      });        
	    }
	  }
	}


}

/* TODO in that case we know who has the card Unsichtbarkeitsschrank: Heike
 *      because Anna has it not and Julia has it not 
 *

Gisela question: --- List(Bellatrix Lestrange (who), Verteidigung gdd Künste (where), Unsichtbarkeitsschrank (what))
Anna answer:	 Some(Bellatrix Lestrange (who))
Julia answer:	 Some(Verteidigung gdd Künste (where))
Heike answer:	 Some(Unsichtbarkeitsschrank (what))
Frank answer:	 None
 do not know what to do with 1 players and 1 cards
Anna: bad cards: List(Unsichtbarkeitsschrank (what))
Julia: bad cards: List(Unsichtbarkeitsschrank (what))
Heike: bad cards: List(Verteidigung gdd Künste (where))
 ... Anna ... Julia ... Heike ... Gisela ... Frank
Bellatrix Lestrange       : --- List(?, ?, ?, ?, -)
Peter Pettigrew           : --- List(1, 1, -, -, -)
Draco Malfoy              : --- List(-, 2, 2, -, -)
Lucius Malfoy             : --- List(2, 2, -, -, -)
Dolores Umbridge          : --- List(-, -, -, -, 1)
Grabbe and Goyle          : --- List(-, -, 1, -, 1)
Große Halle               : --- List(-, 2, 2, 2, -)
Krankenflügel             : --- List(?, ?, ?, ?, ?)
Raum der Wünsche          : --- List(2, 2, 2, -, -)
Zaubertränke              : --- List(?, ?, ?, ?, ?)
Pokalzimmer               : --- List(-, -, 1, -, 1)
Wahrsagen                 : --- List(?, ?, ?, ?, ?)
Eulerei                   : --- List(?, -, -, -, 1)
Bibliothek                : --- List(?, ?, ?, ?, ?)
Verteidigung gdd Künste   : --- List(1, 2, -, -, -)
Petrificus Totalus        : --- List(2, 2, 2, -, -)
Schlafmittel              : --- List(-, 2, 2, 2, -)
Unsichtbarkeitsschrank    : --- List(-, -, 1, -, -)
Portschlüssel             : --- List(-, 3, -, -, -)
Alraune                   : --- List(?, ?, ?, ?, ?)
Impedimenta               : --- List(?, -, -, -, 1)
* 
* 
* 
* TODO
* 
* 
* Frank question: --- List(Draco Malfoy (who), Raum der Wünsche (where), Petrificus Totalus (what))
Anna answer:	 Some(Raum der Wünsche (where))
Julia answer:	 Some(Draco Malfoy (who))
Heike answer:	 None
Gisela answer:	 Some(Petrificus Totalus (what))
the leader Frank (List(---Alraune (what), ---Verteidigung gdd Künste (where), ---Lucius Malfoy (who))) cannot have the cards
solve_2: do not know what to do with 2 players and 2 shown cards and remaining 3 cards
solve_2: do not know what to do with 2 players and 2 shown cards and remaining 3 cards
Anna: bad cards: List(Draco Malfoy (who), Petrificus Totalus (what))
Julia: bad cards: List(Raum der Wünsche (where), Petrificus Totalus (what))
Gisela: bad cards: List(Draco Malfoy (who), Raum der Wünsche (where))
since the card of category 'who' is in the talon 
  a player with only one card of that type must be the owner of that card
 ... Anna ... Julia ... Heike ... Gisela ... Frank
Bellatrix Lestrange       : --- List(-, -, -, -, -)
Peter Pettigrew           : --- List(-, 2, 2, 2, -)
Draco Malfoy              : --- List(-, X, -, -, -)
Lucius Malfoy             : --- List(2, -, 2, -, 2)
Dolores Umbridge          : --- List(-, ?, 2, 2, -)
Grabbe and Goyle          : --- List(2, -, 2, 2, -)
Große Halle               : --- List(-, -, 2, 2, -)
Krankenflügel             : --- List(?, ?, ?, ?, ?)
Raum der Wünsche          : --- List(2, -, -, -, -)
Zaubertränke              : --- List(-, 1, ?, -, -)
Pokalzimmer               : --- List(-, 2, ?, -, ?)
Wahrsagen                 : --- List(-, -, -, 1, -)
Eulerei                   : --- List(-, -, 2, 2, -)
Bibliothek                : --- List(?, ?, ?, ?, ?)
Verteidigung gdd Künste   : --- List(?, ?, ?, ?, ?)
Petrificus Totalus        : --- List(-, -, -, ?, -)
Schlafmittel              : --- List(?, ?, ?, ?, ?)
Unsichtbarkeitsschrank    : --- List(-, 1, -, -, -)
Portschlüssel             : --- List(2, -, 2, -, -)
Alraune                   : --- List(-, 2, ?, -, ?)
Impedimenta               : --- List(-, -, 2, 2, -)
* 
* 
in that case Gisela must have Petrificus totalus (she is the only possible owner)
in that case Anna must have Raum der Wünsche (she is the only possible owner)

Rule: if a shower has two bad cards, the third card must be his own
* 
* 
* 
Bellatrix Lestrange       : --- List(-, -, -, -, X)
Peter Pettigrew           : --- List(2, ?, -, ?, -)
Draco Malfoy              : --- List(-, -, X, -, -)
Lucius Malfoy             : --- List(?, 1, -, -, -)
Dolores Umbridge          : --- List(-, -, X, -, -)
Grabbe and Goyle          : --- List(-, -, -, X, -)
Große Halle               : --- List(X, -, -, -, -)
Krankenflügel             : --- List(?, ?, -, -, ?)
Raum der Wünsche          : --- List(?, ?, ?, ?, ?)
Zaubertränke              : --- List(X, -, -, -, -)
Pokalzimmer               : --- List(-, ?, -, 2, ?)
Wahrsagen                 : --- List(-, -, -, ?, ?)
Eulerei                   : --- List(-, 1, -, -, -)
Bibliothek                : --- List(-, -, ?, -, -)
Verteidigung gdd Künste   : --- List(-, -, X, -, -)
Petrificus Totalus        : --- List(-, -, -, -, X)
Schlafmittel              : --- List(?, 1, -, -, -)
Unsichtbarkeitsschrank    : --- List(-, -, -, -, -)
Portschlüssel             : --- List(-, -, X, -, -)
Alraune                   : --- List(3, ?, -, -, -)
Impedimenta               : --- List(-, X, -, -, -)
* 
* If we have determined all cards of a player (here 4: Heike)
* we can be sure that this player does not have any of the
* other cards in question (here: Bibliothek, Raum der Wünsche)
*
* 
*/