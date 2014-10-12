package cluedo.solver

import cluedo.Player

class Context (val names: List[String]) {
  lazy val testees: List[Testee] = makePlayers(names)

  /** for each name make one Player */ 
  private def makePlayers(names: List[String]) = names.map( { new Testee(_)} );
  
    /** all players but not the ones in the list */
  def other(player: List[Player]): List[Testee] = testees.diff(player);
  def asTestee(player: List[Player]): List[Testee] = testees.filter(player.contains(_));
  def asTestee(player: Player): Player = asTestee(List(player)).head
}