package cluedo.solver

import cluedo.Card

//object Solution {
//  //var ctx: Context = new Context(List());
//  //def setHelper(actx: Context) = ctx = actx
//  
//	def apply(rounds: List[Round]): Solution = {
//    return new SolutionMutualExclusive(rounds);
//  }
// 
//	def apply(addee: Addee, rounds: List[Round]): Solution = {
////		val s = new Solutions
////		val x = new s.SolutionAdd(addee, rounds)
////		return x
//    return new SolutionAdd(addee, rounds);
//	}
//	
//	def apply(removee: Removee, rounds: List[Round]): Solution = {
////		val s = new Solutions
////		val x = new s.SolutionRemove(removee, rounds)
////		return x
//	  return new SolutionRemove(removee, rounds);
//	}
//
//	def solve(removee: Removee, round: Round): Unit = 
//	  Solution(removee, List(round)).solve(round);
//	
//	def solve(addee: Addee, round: Round): Unit = 
//	  Solution(addee, List(round)).solve(round);
//}

class Solutions(val ctx: Context) {
	def solve(removee: Removee, round: Round): Unit = 
	  new SolutionRemove(removee, ctx).solve(round);
	
	def solve(addee: Addee, round: Round): Unit = 
	  new SolutionAdd(addee, ctx).solve(round);
  
	def solve(round: Round): Unit = 
	  new SolutionMutualExclusive(ctx).solve(round);  
}


class Solution(ctx: Context) {
  var solved: Boolean = false;
  
  //def solve: Unit = { rounds.foreach(solve(_)) };
  def solve(rounds: List[Round]): Unit =  { rounds.foreach(solve(_)) };
  def solve(round: Round): Unit = {}
  
  def solve(addee: Addee, round: Round): Unit = new SolutionAdd(addee, ctx).solve(round)
}

//class Solutions {
  
/** Removing a card */	
class SolutionRemove(removee: Removee, ctx: Context) extends Solution(ctx) {
  // if the group in maybe list contains the card the group should be removed from the list
  // er, no. Not really. In fact only this one card should be removed
  override def solve (round: Round) = {
    // TODO add removee on other place
    //rounds.foreach(_.addRemovee(removee))
    
    if (round.shower.length == 3 && round.nCards == 3)  {
      new SolutionRemove3_3(removee, ctx).solve(round); 
    } else if (round.shower.length == 2 && round.nCards == 3 && round.cards.length == 2) {
      new SolutionRemove2_2(removee, ctx).solve(round); 
    } else if (round.shower.length == 1                      && round.cards.length == 1) {
      new SolutionRemove1_1(removee, ctx).solve(round); 
//    } else if (round.shower.length == 0) {
//      new SolutionRemove0(removee, ctx).solve(round); 
    } else if (round.nCards == 2) {
      solve_2(round);
    } else
      println (" do not know what to do with " + round.shower.length + " players and " + round.nCards + " shown cards and remaining " + round.cards.length + " cards"); 
  }

  // nCards = 2 e.g. two showers originally
  def solve_2 (round: Round) = {
     // println ("solve_2: do not know what to do with " + round.shower.length + " players and " + round.nCards + " shown cards and remaining " + round.cards.length + " cards"); 
     // if A has not card 1 and B has not card 1
     //    then A has card 2 and B has card 3 or vice versa
     // Solving possible if
     //   - A has not card 2 or 3
     //   - B has not card 2 or 3
     //   - A has card 2 or 3
     //   - B has card 2 or 3
     //val solution = new SolutionMutualExclusive(rounds: List[Round]);
     //solution.solve(round);
    
//      val missingCardsInCommon = cardsA.intersect.cardsB
//      if (round.shower.length == 3)  {
//        new SolutionRemove3_3(removee, rounds).solve(round); 
    
  }

}

class SolutionRemove3_3(removee: Removee, ctx: Context) extends SolutionRemove(removee, ctx) {
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
    println ("SolutionRemove3_3: what to do with " + round.removees.length + " removees and " + round.addees.length + " addees");
    //println("The leader " + round.leader + " cannot have the cards");
    
    if (round.removees.length > 2) println("detail: " 
        + round + " r: " + round.removees);
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
    val player = ctx.asTestee(round.shower)
    round.cards.foreach (card => {
      val notOwners: List[Testee] = player.filter(_.notOwnedCards.contains(card));
      val remainingPlayers = player.diff(notOwners);
      if (remainingPlayers.length == 1 && round.nCards == 3) {
        println("two players " + notOwners 
            + " have not the card " + card + " thus the remaining one (" + remainingPlayers + ") must have the card");
        remainingPlayers.foreach(p => solve(new Addee(p, card), round))
        // TODO and accordingly Heike and Gisela have the other two cards mutually exclusive
        new SolutionMutualExclusive(ctx).solve(round);
      }
    });
  }

}

/** 
 * Solution Remove 2_2
 * <p> Players: A, B, cards: 1, 2 </p>
 * if player A owns card 1 the player B owns card 2 and vice versa
 */
class SolutionRemove2_2(removee: Removee, ctx: Context) extends SolutionRemove(removee, ctx) {
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
    solved = true;
    // this round is solved, the result is in the players
  }
}

// unused, very rare
/** 
 * This player was the only one who has shown a card (one of three) 
 * and now one (of three) has been removed
 * thus two are left
 */
class SolutionRemove1_1(removee: Removee, ctx: Context) extends SolutionRemove(removee, ctx) {
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

// unused, very very rare
//class SolutionRemove0(removee: Removee, rounds: List[Round]) extends SolutionRemove(removee, rounds) {
//  // if there are two players and two cards
//  override def solve (round: Round) = {
//    //println("SolutionRemove0");
//    //println (" do not know what to do with " + round.shower.length + " players and " + round.nCards + " cards");
//    println (" this is a rare case. 0 players and cards: " + round.cards);
//    // solving possible if
//    //   - the leader has none of the card
//    //     but in that case no solving is required
//    //rounds.foreach {_.addSolution(this)}
//  }
//}

/** We have found a player who has a card */
class SolutionAdd(addee: Addee, ctx: Context) extends Solution(ctx) {
  override def solve (round: Round) = {
    if (round.shower.length == 2 && round.nCards == 3 && round.cards.length == 2) {
      new SolutionAdd2_2(addee, ctx).solve(round);
    } else {
      new SolutionAdd1(addee, ctx).solve(round);      
    }
  }
}

class SolutionAdd1(addee: Addee, ctx: Context) extends Solution(ctx) {
  override def solve (round: Round): Unit = {
    // if the player has already the card we do not need to add it a second time
    if (addee.player.cards.contains(addee.card))
      return
    println("SolutionAdd: have " +  round );
    if (round.shower.length == 2 && round.nCards == 3 && round.cards.length == 3)
      println("we have 2 players and two cards, we can do something: \n  " + round.dump)
    // we have found a player who has a card
    ctx.asTestee(addee.player).addCard(addee.card)
    println("SolutionAdd: player: " + addee.player +  "  has got card: " + addee.card);
    // since this player has the card, all other players do not have that card
    val others: List[Testee] = ctx.other(List(addee.player));
    others.foreach(_.markHasNot(List(addee.card)))
    // make a note
    round.addAddee(addee);
  }
}

/** Adding two players, two cards */
class SolutionAdd2_2(addee: Addee, ctx: Context) extends SolutionAdd(addee, ctx) {
  // if there are two players and two cards
  override def solve (round: Round) = {
    println("SolutionAdd2_2");
    // add to the first round and then add remaining player
    new SolutionAdd1(addee, ctx).solve(round);
    val cards: List[Card] = round.cards;
    // Players: A, B, cards: 1, 2
    // if player A owns card 1 the player B owns card 2
    val lastCard: Card = cards.diff(List(addee.card)).head
    val lastPlayer = round.shower.diff(List(addee.player)).head
    // so, the last card is owned by the last player
    //   and this is the beginning of a new solution
    new SolutionAdd1(new Addee(lastPlayer, lastCard), ctx).solve(round);
    solved = true;
    // this round is solved, the result is in the players
  }
}

// two players, two cards
class SolutionMutualExclusive(ctx: Context) extends Solution(ctx) {
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
	        new SolutionRemove2_2(new Removee(player, missingCards.head), ctx).solve(round);
	      } 
      });        
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
* 
* 
*/