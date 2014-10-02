package cluedo.solver

import cluedo._;

class RoundListener(game: cluedo.Round) extends cluedo.Listener {
  val solver: Solver = new Solver(game);
  
  override  def action(player: Player, www: List[Card]): Unit = {
    super.action(player, www);
    // compute the results of the former round
    solver.run
    solver.add(new Question(player, www));
  }      
  
  override def action(player: Player, www: Option[Card]): Unit = {
    super.action(player, www);
    solver.add(new Answer(player, www));
  } 

}

class Question (val player: Player, val www: List[Card]) {  }

class Answer (val player: Player, val www: Option[Card]) {  }

class Round(val qst: Question) {
  /** get leader who has asked the question */
  val leader: Player = qst.player;
  var ansList: List[Answer] = List();
  def add(ans: Answer) = ansList = ans :: ansList;
  /** get all cards that have been asked for */
  def cards: List[Card] =
    // everybody knows the cards the leader has asked for
    qst.www
    // remove the cards we have already sorted out
    .diff(removedCards);
  /** get all cards shown, remove options (but you know only the amount) */
  def nCards: Int =  ansList.map(_.www).flatten.length;
  /** get all players which have shown a card */
  def shower: List[Player] =
    // remove empty options, e.g. players who have not shown a card
    ansList.filter(_.www.isDefined) .map {_.player}
    // remove the players we have already sorted out
    .diff(removedPlayers)
  /** return the shower list if the card matches */
  def getPlayersFor(card: Card): List[Player] = if (cards.contains(card)) shower else List()
  // there are four types of groups:
  // a) 3 player - the cards are not in the talon
  // b) 2 player - the players possess the cards mutually exclusive
  // c) 1 player - player is in possession of one (or more) cards 
  // d) 0 player - cards are in talon or on hand of asker
  // Groups will be worked on:
  // If a card has been assigned to a player it will be removed from the optional other players.
  // E.g. a 2 player will become a 1 player type
  var removedCards: List[Card] = List()
  var removedPlayers: List[Player] = List()
  def removeCard(card: Card) = removedCards = card :: removedCards
  def removePlayer(player: Player) = removedPlayers = player :: removedPlayers
  def remove(player: Player, card: Card) = { removePlayer(player); removeCard(card); }
    
  var removees: List[Removee] = List();
  def addRemovee(removee: Removee) = removees = removee :: removees;
    
  var solutions: List[Solution] = List();
  def addSolution(sol: Solution) = solutions = sol :: solutions
  //def spreadSolutions(sol: Solution, player: Player, card: Card) = getPlayersFor(card).foreach {_.addSolution(sol)}
  
}

	
class Removee(val card: Card, val player: Testee) {}
class Addee(val card: Card, val player: Player) {}

class Testees (val names: List[String]) {
  lazy val testees: List[Testee] = makePlayers(names)

  /** for each name make one Player */ 
  private def makePlayers(names: List[String]) = names.map( { new Testee(_)} );
  
    /** all players but not the ones in the list */
  def other(player: List[Player]): List[Testee] = testees.diff(player);
  def asTestee(player: List[Player]): List[Testee] = testees.filter(player.contains(_));
  def asTestee(player: Player): Player = asTestee(List(player)).head
}

class Testee(name: String) extends Player(name: String) {
  var notOwnedCards: List[Card] = List()
  /** List with two or three cards and the player owns one of them */
  var ownedMaybeList: List[Round] = List()
  //var solutions: List[Solution] = List();
  def markHasNot(cards: List[Card]) = {
    // mark has not
    notOwnedCards = cards ::: notOwnedCards;
    // and remove those cards from the maybe list
    cards.foreach(removeMaybe(_))
  }
  // add cards (rounds) to maybe list but not if the card is already on the 'has not' list
  def markMaybeLists(round: Round) = {
    // is it already clear that this player does not own the card?
    val badCards = round.cards.intersect(notOwnedCards)
    if (!badCards.isEmpty)
      println (name +": bad cards: " + badCards)
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
      // this player is part of all those groups because it his own list
      //val updatingGroups: List[List[Player]] =  getMaybeCardGroups(card);
      //if (!updatingGroups.isEmpty)
      //  println("updating groups: " + updatingGroups)
      // the updating group should only contain one element
      
      // TEST
      //val len = ownedMaybeList.length;
      // if the group in maybe list contains the card the group should be removed from the list
      // er, no. Not really. In fact only this one card should be removed
//      ownedMaybeList = ownedMaybeList.filter(!_.cards.contains(card))
//      if (len > ownedMaybeList.length)
//        println (name + ": maybe has been removed: " + ownedMaybeList.map(_.cards))
      val removee: Removee = new Removee(card, this);
//      val sol: Solution = new SolutionRemove(removee, ownedMaybeList.filter(_.cards.contains(card)));
      val sol: Solution = Solution(removee, ownedMaybeList.filter(_.cards.contains(card)));
      //solutions = sol :: solutions;
      sol.solve;
  }
  
  override def toString() = super.toString() + "\n  not owned cards: " + notOwnedCards.distinct + "\n";
}


class Analyser(names: List[String]) {
  // 1. if a player has a card optionally in one round but not in an other round
  //   the player has not the card
  // 2. in that case the remaining players of the group are reduced
  // find the groups with same question cards
  val helper: Testees = new Testees(names);
  cluedo.solver.Solution.setHelper(helper);
  val testees: List[Testee] = helper.testees;
  
  def search(round: Round) = {
    markPlayerHasNot(round);
    // mark for each player (including leader) that he (may) have this card(s)
    //    but not if the player already 'has not' this card
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

  def markPlayerHasNot(round: Round) = {
    def otherPlayers: List[Testee] = {
	    if (round.nCards == 0) {
	      println ("other: " + helper.other(List(round.leader)))
	      return helper.other(List(round.leader))
	    } else {
	      //println ("shower: " + round.shower )
	      val players: List[Player]= round.leader :: round.shower;
	      return helper.other(players)
	    }
    }
    
    otherPlayers.foreach(_.markHasNot(round.cards))
  }

  /** three, two or one player may have all three cards */
  def markPlayerMayHave(round: Round) = {
    var players: List[Testee] = List()
    if (round.nCards == 3) {
      players = helper.asTestee(round.shower);
    }
    else if (round.nCards > 0) {
      players = helper.asTestee(round.leader :: round.shower);
    }
    else if (round.nCards == 0) {
      players = helper.asTestee(List(round.leader));
      // TODO and talon
    }
    players.foreach {_.markMaybeLists(round)};
    
  }
  
  def combine(round: Round, rounds: List[Round]) {
    // if the player is in more than one group which may have this card
    //   we may identify this player as the owner of the card
    //   1. the other players must be re-calculated
    //  Example: cards A B C   and C D E (C is common and each time player 1 shows a card)
    
    // players 1 - 3
    val players = round.shower;
    val cards = round.cards;
    rounds.foreach (r => {
      val commonCards: List[Card] = r.cards.intersect(cards);
      // just one card?  A B  - C - D E
      if (commonCards.length == 1) {
        val card = commonCards.head;
        println ("found just one card: " + card);
        // search common player
        val commonPlayers = r.shower.intersect(players)
        if (commonPlayers.length == 1) {
          val player = commonPlayers.head;
          println("found just one player: " + commonPlayers.head + " and adding card " + commonCards.head);
          helper.asTestee(player).addCard(card);
          round.remove(player, card);
          r.remove(player, card);
        }
      }
    });
  }
  
  def showCard(card: Card): List[String] = testees.map { _.showCard(card) }
  
  override def toString() = "  testees: " + testees
}

class Solver(game: cluedo.Round) {
  
  val watson = new Analyser(game.group.players.map(_.name));
  var rounds: List[Round] = List();  
  
  def add(qst: Question) = rounds = new Round(qst) :: rounds;
  def add(ans: Answer) = rounds.head.add(ans)
  def run = if (!rounds.isEmpty) {
    watson.search(rounds.head); 
    watson.combine(rounds.head, rounds.tail)
    showResult;
  }
  
  def showResult = {
    header; 
    println;
    body;
    //println ("watson: " + watson)
  }
  
  def names: List[String] = game.group.players.map(_.name)
  def header = game.group.names.foreach { n => print( " ... " + n) }; println;
  val cards: List[Card] = new cluedo.Cards().all;
  def body   = cards.foreach { c => line(c) };
  def fill(len: Int): String = "                          ".substring(len)
  def line(card: Card) = print (card.name + fill(card.name.length) + ": ---" + playersHave(card) + "\n");
  def playersHave(card: Card): String = " " + watson.showCard(card)

}