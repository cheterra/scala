package cluedo

import com.sun.org.glassfish.external.statistics.Stats

class Statistics (var left: Int) {  
//  var index: Int = -1
    var restartIdx: Int = -1
    var counter = 1
    var minLeft: Int = left
    var sumLeft: Int = left;
    
    def touch = {
      counter = counter + 1;
      minLeft = math.min(minLeft, left)
    }

    def average: Int = sumLeft / counter;
    def update(left: Int) = {
      this.left = left;
      sumLeft = sumLeft + left;
      minLeft = math.min(minLeft, left)
    }
    
    override def toString  (): String = "[ idx: " + restartIdx+ ", c: " + counter + ", l: " + left + ", min: " + minLeft + ", av: " + average + "]"
}

class Game (val stat: Statistics, val moves: List[RealMove]) {
  stat.restartIdx = moves.size / 2;
  println("new game with moves: " + moves.size + " and stat: " + stat)
  
  def update(left: Int) = stat.update(left)
}

class Strategy (games: List[Game], mod: Model) {
  def getBestGames: List[Game] = return games;
  // since we prepend game(0) is usually the most recent game
  def getGame: Game = return games(0);
  def getMoves: List[RealMove] = return games(0).moves;
  def getDoneMoves: List[RealMove] = List()
}

// do half of the moves (half of the game)
// not used
class MediumStrategy (games: List[Game], mod: Model) 
  extends Strategy(games, mod) {

  override def getGame: Game = {
    val game = games(0);
    val medium = getMoves.size / 2;

    var m = 0;
    for (m <- 0 to medium) {
      //println("move: " + m + ": " + moves(m));
      mod.doMove(getMoves(m))
    }
    return game
  }
}

/** do half of the moves (half of the game) and reduce the data */
class MedRedStrategy (games: List[Game], mod: Model) 
  extends Strategy(games, mod) {

  var medium = 0
    
  override def getGame: Game = {
    val game = games(0);
    val stat = game.stat
    // copy restart index from last game
    if (stat.counter == 1 && games.size > 1)
      stat.restartIdx = games(1).stat.restartIdx
    if (stat.left < 3 && stat.average <= 4 && stat.counter > 5 && stat.restartIdx < 20)
      stat.restartIdx = stat.restartIdx + 1;
    medium = stat.restartIdx;
    //medium = getMoves.size / 2;
    
    var m = 0;
    for (m <- 0 to medium) {
      // begin with m moves which we have judged to be good
      // TEST if move is possible (it should)
      if (!mod.canMove(getMoves(m)))
        println("error: move " + m + " " + getMoves(m) + " not possible")
      mod.doMove(getMoves(m))
    }
    return game
  }
  
  override def getDoneMoves: List[RealMove] = getMoves.slice(0, medium)
  
  
  override def getBestGames: List[Game] = {
    // if the average results are too bad remove that game
    if (games.size >= 1 && games(0).stat.average >= 6)
      return games.tail;

    // remove games which are not so good
    if (games.size > 1 && games(0).stat.minLeft > 3)
      return games.tail;

    return games
  }
}

/**
 * Try to get just one
 */
class JustOne (mod: Model){
  var bestGames: List[Game] = List()
  /** since we prepend the moves they are in the wrong order */
  var moves: List[RealMove] = List()
  
  def select(): Unit = {
    moves = List()
    if (bestGames.size > 0) {
      dumpGames;
      //println("best games size = " + bestGames.size + ", stat: " + bestGames(0).stat)
      var strat: Strategy = new MedRedStrategy(bestGames, mod);
      bestGames = strat.getBestGames;
      strat.getGame.stat.touch;
      // the other moves will be added. We must preset the moves already done.
      moves = strat.getDoneMoves.reverse;
    }
  }
  
  def add(mv: RealMove) = { 
    moves = mv :: moves; 
    //println(moves.size + " added move: " + mv) 
  }
  
  def reset: Unit = { moves = List() } 
  
  def improve(left: Int): Unit = {
    println("improve: " + left);
    // if this game was resulting in just one stick left over wait a moment
    if (left == 1)
      Thread.sleep(30000);
    // since we prepend the moves they are in the wrong order (reverse)
    if (left < 4) 
      bestGames = new Game(new Statistics(left), moves.reverse) :: bestGames;
    else if (bestGames.size > 0)
      bestGames(0).update(left)
     
    reset    
  }
  
  def dumpGames = bestGames.foreach { game =>
    println("best games size = " + bestGames.size + ", stat: " + game.stat)
  }   
}