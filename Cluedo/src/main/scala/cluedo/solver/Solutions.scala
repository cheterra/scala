package cluedo.solver

import cluedo.Card

object Solution {
  var helper: Testees = new Testees(List());
  def setHelper(aHelper: Testees) = helper = aHelper
  
	def apply(rounds: List[Round]) = 		new Solutions
 
	def apply(addee: Addee, rounds: List[Round]): Solution = {
		val s = new Solutions
		val x = new s.SolutionAdd(addee, rounds)
		return x
	}
	
	def apply(removee: Removee, rounds: List[Round]): Solution = {
		val s = new Solutions
		val x = new s.SolutionRemove(removee, rounds)
		return x
	}

}

  class Solution(rounds: List[Round]) {
	  var solved: Boolean = false;
	  
    //var removees: List[Removee] = List();
    
    def solve: Unit = { rounds.foreach(solve(_)) };
    def solve(round: Round): Unit = {}
	}

class Solutions {

	
class SolutionRemove(removee: Removee, rounds: List[Round]) extends Solution(rounds) {
    // if the group in maybe list contains the card the group should be removed from the list
    // er, no. Not really. In fact only this one card should be removed
    override def solve (round: Round) = {
      rounds.foreach(_.addRemovee(removee))
      //val len = rounds.length;
      //rounds.foreach() {_.addSolution(this)}
      // if the group in maybe list contains the card the group should be removed from the list
      // er, no. Not really. In fact only this one card should be removed
//      case 0: solved = true; // this is solved
//      case 1: 
      
//
//      cards.len
//      case 0: // this is solved
//      case 1: card == card ? no? error
//                             yes? Yeah! 

//      if (len > ownedMaybeList.length)
//        println (name + ": maybe has been removed: " + ownedMaybeList.map(_.cards))

//      round match {
//        case _ if round.shower.length == 3 && round.nCards == 3 && round.removees.length > 1 =>
//          { new SolutionRemove3_3(removee, rounds).solve(round); }
//        case _ if round.shower.length == 2 && round.nCards == 3 && round.cards.length == 2 =>
//          {  new SolutionRemove2_2(removee, rounds).solve(round); }
//        case _ => println (" do not know what to do with " + round.shower.length + " players and " + round.nCards + " cards); 
//      }
      
        if (round.shower.length == 3 && round.nCards == 3 && round.removees.length > 1)  {
          new SolutionRemove3_3(removee, rounds).solve(round); 
        } else if (round.shower.length == 2 && round.nCards == 3 && round.cards.length == 2) {
          new SolutionRemove2_2(removee, rounds).solve(round); 
        }
        else
          println (" do not know what to do with " + round.shower.length + " players and " + round.nCards + " cards"); 

    }
}

class SolutionRemove3_3(removee: Removee, rounds: List[Round]) extends SolutionRemove(removee, rounds) {
  // there is nothing we can do with one of this. Need a second one
    override def solve (round: Round) = {
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
    }
  
}

class SolutionRemove2_2(removee: Removee, rounds: List[Round]) extends SolutionRemove(removee, rounds) {
    // if there are two players and two cards
    override def solve (round: Round) = {
      println("SolutionRemove2_2");
      val cards: List[Card] = round.cards;
      // Players: A, B, cards: 1, 2
      // if player A owns card 1 the player B owns card 2 and vice versa
      val lastCard: Card = cards.diff(List(removee.card)).head
      val lastPlayer = round.shower.diff(List(removee.player)).head
      // so, the last card is owned by the removee
      Solution.helper.asTestee(removee.player).addCard(lastCard);
      println("Solution2_2: player 1: " + removee.player +  " has got card:\n" + lastCard)
      // and the removed card must be owned by the last player
      Solution.helper.asTestee(lastPlayer).addCard(removee.card)
      println("Solution2_2: player 2: " + lastPlayer +  " has got card:\n " + removee.card)
      // and this is the beginning of a new solution
      val solution = new SolutionAdd(new Addee(removee.card, lastPlayer), rounds);
      solution.solve;
      // TODO that solutions should be added to
      //   1. all groups with this player
      //   2. all groups with this card
      val lastRounds = rounds.diff(List(round))
      lastRounds.foreach {_.addSolution(solution)}
      solved = true;
      // this round is solved, the result is in the players
    }
}

class SolutionAdd(addee: Addee, rounds: List[Round]) extends Solution(rounds) {

}

class SolutionAdd2_2(addee: Addee, rounds: List[Round]) extends SolutionAdd(addee, rounds) {
    // if there are two players and two cards
    override def solve (round: Round) = {
      println("SolutionAdd2_2");
      val cards: List[Card] = round.cards;
      // Players: A, B, cards: 1, 2
      // if player A owns card 1 the player B owns card 2 and vice versa
      val lastCard: Card = cards.diff(List(addee.card)).head
      val lastPlayer = round.shower.diff(List(addee.player)).head
      // so, the last card is owned by the last player
      Solution.helper.asTestee(lastPlayer).addCard(lastCard);
      println("SolutionAdd2_2: player: " + lastPlayer +  " has got card: " + lastCard)
      // and this is the beginning of a new solution
      val solution = new SolutionAdd(new Addee(lastCard, lastPlayer), rounds);
      solution.solve;
      rounds.foreach {_.addSolution(solution)}
      solved = true;
      // this round is solved, the result is in the players
    }
}

//class SolutionAdd3_3(addee: Addee, rounds: List[Round]) extends SolutionAdd(addee, rounds) {
//    // if there are three players and three cards -> remove a card and a player
//    override def solve (round: Round) = {
//      // remove a player and a card 
//      round.removeCard(addee.card)
//      round.removePlayer(addee.player)
//      println("addee: " + addee.player + "/" + addee.card + " removed from round")
//    }
//}

//case class SolutionClass(players: Int, cards: Int, sol: Solution)

//	class Solutions {
//	  def solve(round: Round) = {
//	    val nCards: Int = round.nCards;
//	    val solution: Solution = nCards match {
//	      case 1 => new Solution1(round)
//	      case 2 => new Solution2(round)
//	      case 3 => new Solution3(round)
//	    }
//	  }
//	}
	

}