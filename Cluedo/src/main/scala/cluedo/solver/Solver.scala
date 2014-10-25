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
    
  // used to reduce the 'shower' list
  var removees: List[Removee] = List();
  def addRemovee(removee: Removee) = removees = (removee :: removees).removeDuplicates;

  // not used
  var addees: List[Addee] = List();
  def addAddee(addee: Addee) = addees = (addee :: addees).removeDuplicates;

  override def toString() = "showers left: " + shower.length + ", cards shown: " + nCards + ", cards left: " + cards.length + ", rem: " + removees.length + ", add: " + addees.length;
  def dump: String = "round:\n-  shower: " + shower + ",\n-  cards left: " + cards;

}

	
class Removee( val player: Testee, val card: Card) {}
class Addee(val player: Player, val card: Card) {}

class Solver(game: cluedo.Round) {
  
  def names: List[String] = game.group.players.map(_.name)
  val cards: List[Card] = new cluedo.Cards().all;
  val watson = new Analyser(names, cards);
  var rounds: List[Round] = List();
  // set amount of owned cards
  val pairs = watson.testees.zip(game.group.players);
  pairs.foreach ( pair => { val (t, p) = pair; t.nCards = p.cards.length;} )
  
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