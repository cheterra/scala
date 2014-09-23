package cluedo


/** A Player - has a name and may have got cards */
class Player(val name: String) {
  var cards: List[Card] = Nil;
  def addCard(c: Card) = { cards = c +: cards }

  /** Show one of my cards which are demanded. None if there is not match */
  def showOneOf(demand: List[Card]): Option[Card] = cards.intersect(demand).headOption

  override def toString() = name + " (" + cards.map( "---" + _).toString + ")";
}

/** The players, have a leader and his opponents, can choose next leader */
class Players (val names: List[String]) {
  lazy val players: List[Player] = makePlayers(names)
  var activePlayer: Iterator[Player] = players.iterator;
  var leader: Player = next;

  /** for each name make one Player */ 
  private def makePlayers(names: List[String]) = names.map( { new Player(_)} );
  
  /** one after another and then from the beginning - clockwise */
  def next: Player = {
    // reset to the beginning? e.g. next round is starting
    if (!activePlayer.hasNext)
      activePlayer = players.iterator
    activePlayer.next()
  }
  
  def nextLeader = { leader = next }
  /** all players but not the leader */
  def other: List[Player] = players.diff(List(leader));
  
  override def toString() = players.map( _ + ",\n").toString;
}
