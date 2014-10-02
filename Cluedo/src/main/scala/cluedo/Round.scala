package cluedo


/** Will be informed about actions in the round */
class Listener {
  def action(player: Player, www: List[Card]): Unit = 
    println(player.name + " question: --- " + www);
  
  def action(player: Player, www: Option[Card]): Unit = 
    println(player.name + " answer:\t " + www);
}

/** One round, leader is asking, other players responding */
class Round (val group: Players, val cards: Cards) { 
  
  var lis: Listener = new Listener;
  
  /** demand random three cards, one from each category */
  private def demand: List[Card] = return cards.random3;
  
  /** One player must answer if he can */
  private def answer(player: Player, www: List[Card]):
    Option[Card] = player.showOneOf(www)
  
  /** all other players must answer if they can */
  def answerLoop(www: List[Card] ) = 
    group.other.foreach(p => lis.action(p, answer(p, www)) ); 
  
  
  def run = {
    val www = demand;
    lis.action(group.leader, www)
    answerLoop(www);
    group.nextLeader
  };

}