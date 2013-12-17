package solitaire

class Statistics (val left: Int) {  
}

class Game (val stat: Statistics, val moves: List[RealMove]) {
}

class Strategy (games: List[Game], mod: Model) {
  // since we prepend this is usually the most recent game
  def get: Game = return games(0);
}

// do half of the moves (half of the game)
class MedianStrategy (games: List[Game], mod: Model) extends Strategy(games, mod) {
  override def get: Game = {
    val game = games(0);
    // since we prepend the moves they are in the wrong order
    val moves =  game.moves.reverse;
    val median = moves.size / 2;

    var m = 0;
    for (m <- 0 to median) {
      //println("move: " + m + ": " + moves(m));
      mod.doMove(moves(m))
    }
    return game
  }
}

/**
 * Try to get just one
 */
class JustOne (mod: Model){
  var bestGames: List[Game] = List()
  var moves: List[RealMove] = List()
  
  def select(): Seq[RealMove] = {
    if (bestGames.size > 0) {
      println("best games size = " + bestGames.size)
      moves = new MedianStrategy(bestGames, mod).get.moves;
      // the other moves will be added. We must preset the moves already done.
      return moves
    }
    else
      return mod.getAllMoves();
  }
  
  def add(mv: RealMove) = { 
    moves = mv :: moves; 
    //println("added move: " + mv) 
  }
  
  def reset: Unit = { moves = List() } 
  
  def improve(left: Int): Unit = {
    println("improve " + left);
    if (left < 5) 
      bestGames = new Game(new Statistics(left), moves) :: bestGames
    reset    
  }
}