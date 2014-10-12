package cluedo.solver

import cluedo._;

class RoundListener(game: cluedo.Round) extends cluedo.Listener {
  val solver: Solver = new Solver(game);
  
  override  def action(player: Player, www: List[Card]): Unit = {
    // compute the results of the former round
    solver.run
    super.action(player, www);
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
  lazy val nCards: Int =  ansList.map(_.www).flatten.length;
  
  /** get all players which have shown a card */
  def shower: List[Player] =
    // remove empty options, e.g. players who have not shown a card
    ansList.filter(_.www.isDefined) .map {_.player}
    // remove the players we have already sorted out
    .diff(removedPlayers)
  // it may show up that the leader has not of the cards
  var leaderHasNone: Boolean = false;
  // if the initially showers = 3 or the leader has none of the cards
  def isShowersRound: Boolean = nCards == 3 || leaderHasNone
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
  private def removeCard(card: Card) = removedCards = (card :: removedCards).removeDuplicates
  private def removePlayer(player: Player) = removedPlayers = (player :: removedPlayers).removeDuplicates
  def remove(player: Player, card: Card) = { removePlayer(player); removeCard(card); }
    
  var removees: List[Removee] = List();
  def addRemovee(removee: Removee) = removees = (removee :: removees).removeDuplicates;

  var addees: List[Addee] = List();
  def addAddee(addee: Addee) = addees = (addee :: addees).removeDuplicates;

  override def toString() = "showers left: " + shower.length + ", cards shown: " + nCards + ", cards left: " + cards.length + ", rem: " + removees.length + ", add: " + addees.length;
  def dump: String = "round:\n-  shower: " + shower + ",\n-  cards left: " + cards;

}

	
class Removee( val player: Testee, val card: Card) {}
class Addee(val player: Player, val card: Card) {}

//class Testees (val names: List[String]) {
//  lazy val testees: List[Testee] = makePlayers(names)
//
//  /** for each name make one Player */ 
//  private def makePlayers(names: List[String]) = names.map( { new Testee(_)} );
//  
//    /** all players but not the ones in the list */
//  def other(player: List[Player]): List[Testee] = testees.diff(player);
//  def asTestee(player: List[Player]): List[Testee] = testees.filter(player.contains(_));
//  def asTestee(player: Player): Player = asTestee(List(player)).head
//}

class Testee(name: String, sol: Solutions) extends Player(name: String) {
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
        if (round.nCards == 2)
          sol.solve(round);
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
    rounds.foreach( round => {round.addRemovee(removee); sol.solve(removee, round) })
    
  }
  
  override def toString() = super.toString() + "\n  not owned cards: " + notOwnedCards.distinct + "\n";
}


class Analyser(names: List[String], cards: List[Card]) {
  // 1. if a player has a card optionally in one round but not in an other round
  //   the player has not the card
  // 2. in that case the remaining players of the group are reduced
  // find the groups with same question cards
  //val ctx: Testees = new Testees(names);
  val ctx = new Context(names)
  //cluedo.solver.Solution.setHelper(ctx);
  val testees: List[Testee] = ctx.testees;
  val sol: Solutions = new Solutions(ctx);
  
  // this is called for each new round
  def investigate(round: Round) = {
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
        // if the number of cards was 0 in that round then none 
        //    of the other players has the card (apart the leader)
	      println ("other: " + ctx.other(List(round.leader)))
	      return ctx.other(List(round.leader))
	    } else if (round.nCards == 3) {
	      // he players showing the card may have the cards
	      //   but the leader cannot have the cards
	      println ("the leader " + round.leader + " cannot have the cards")
	      return ctx.other(round.shower)
	    } else {
	      // the players showing the card and the leader may have the
	      //   card but all players who did not show a card have not the card.
	      //println ("shower: " + round.shower )
	      val players: List[Player]= round.leader :: round.shower;
	      return ctx.other(players)
	    }
    }
    
    otherPlayers.foreach(_.markHasNot(round.cards))
  }

  /** three, two or one player may have all three cards */
  def markPlayerMayHave(round: Round) = {
    var players: List[Testee] = List()
    // three players may have the card but not the leader
    if (round.nCards == 3) {
      players = ctx.asTestee(round.shower);
    }
    // two players and the leader and the talon may have the card
    else if (round.nCards > 0) {
      players = ctx.asTestee(round.leader :: round.shower);
      // TODO and talon
    }
    // the leader and the talon may have the card but none of the players
    else if (round.nCards == 0) {
      players = ctx.asTestee(List(round.leader));
      // TODO and talon
    }
    players.foreach {_.markMaybeLists(round)};
    
  }
  
  // this is called for each new round
  def combine(round: Round, rounds: List[Round]) {
    // if the player is in more than one group which may have this card
    //   we may identify this player as the owner of the card
    //  Example: cards A B C   and C D E (C is common and each time player 1 shows a card)
    
    // players 1 - 3
    val players = round.shower;
    val cards = round.cards;
    rounds.foreach (r => {
      val commonCards: List[Card] = r.cards.intersect(cards);
      /*
       *
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
      // if the category matches than assign the card to the owner
      analysers.foreach(_.assign(round, talon.cat))
    });
  }

  def showCard(card: Card): List[String] = testees.map { _.showCard(card) }
  
  override def toString() = "  testees: " + testees
}

class AnalyseTalonCard(testees: List[Testee], val card: Card, val sol: Solutions) {
  
  /** testees which are not the owner of this card */
  val notOwners: List[Testee] =  testees.filter(_.notOwnedCards.contains(card))
  /** card with 0 players must be in talon */
  lazy val isTalonCard: Boolean = notOwners.length == testees.length
  /** card with one possible owner */
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
    println("since the card of category '" + talonCat 
        + "' is in the talon a player with only one card of that type must be the owner of that card");
    possiblyOwner.foreach(owner => sol.solve(new Addee(owner, card), round))
  }
  
}

class Solver(game: cluedo.Round) {
  
  def names: List[String] = game.group.players.map(_.name)
  val cards: List[Card] = new cluedo.Cards().all;
  val watson = new Analyser(names, cards);
  var rounds: List[Round] = List();  
  
  def add(qst: Question) = rounds = new Round(qst) :: rounds;
  def add(ans: Answer) = rounds.head.add(ans)
  def run = if (!rounds.isEmpty) {
    watson.investigate(rounds.head); 
    watson.combine(rounds.head, rounds.tail)
    watson.analyseTalon(rounds.head);
    showResult;
  }
  
  def showResult = {
    header; 
    println;
    body;
    //println ("watson: " + watson)
  }
  
  def header = game.group.names.foreach { n => print( " ... " + n) }; println;
  def body   = cards.foreach { c => line(c) };
  def fill(len: Int): String = "                          ".substring(len)
  def line(card: Card) = print (card.name + fill(card.name.length) + ": ---" + playersHave(card) + "\n");
  def playersHave(card: Card): String = " " + watson.showCard(card)

}