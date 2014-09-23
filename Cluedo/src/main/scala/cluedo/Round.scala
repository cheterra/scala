package cluedo



class Round (group: Players, cards: Cards) { 
  
  /** demand random three cards, one from each category */
  private def demand: List[Card] = return cards.random3;
  
  /** One player must answer if he can */
  private def answer(player: Player, www: List[Card]):
    Option[Card] = player.showOneOf(www)
  
  /** all other players must answer if they can */
  def answerLoop(www: List[Card] ) = {
    group.other.foreach(p => store(p, answer(p, www)) ); 
  }
  
  def store(player: Player, www: List[Card]): Unit = 
    println(player.name + " question: --- " + www);
  
  def store(player: Player, www: Option[Card]): Unit = 
    println(player.name + " answer:\t " + www);
  
  def run = {
    val www = demand;
    store(group.leader, www)
    answerLoop(www);
    group.nextLeader
  };

}