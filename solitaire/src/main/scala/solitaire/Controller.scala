package solitaire

class Controller {
  
  val mod: Model = new Model()

  /** the sticks which are left at the end of the game,
   *  and minimum means that we adjust this value after each game
   */  
  var minTiles = 6 * 6;

  def rand(n: Int) = (Math.random * n).toInt
  
  // Action!

  /** do one possible (the first one, reproducible) move */
  // not used
  def doOnePossibleMove() {
    val moves = mod.getAllMoves()
    if (!moves.isEmpty) {
      val n = 0
      println ("Move: " + moves(n))
      mod.doMove(moves(n))
    }
  }

  /** do one random possible move */
  // not used
  def doRandomPossibleMove() {
    val moves = mod.getAllMoves()
    if (!moves.isEmpty) {
      val n = rand(moves.size)
      println ("Move: " + moves(n))
      mod.doMove(moves(n))
    }
  }

  /** do one random move and restart when no moves are left, calc min. tiles */
  // not used
  def doRandomRepeatedMove() {
    val moves = mod.getAllMoves()
    if (!moves.isEmpty) {
      val n = rand(moves.size)
      println ("Move: " + moves(n))
      mod.doMove(moves(n))
      println ("Min. tiles: " + minTiles)
    }
    else
    // next try
    {
      minTiles = Math.min(minTiles, mod.getAllTiles().size)
      if (minTiles > 3) mod.reset
    }
  }

  /** do all moves until there are no possible moves left */
  def doAllPossibleMoves() {
    //area = getArea
    mod.reset
    var moves = mod.getAllMoves()
    while (!moves.isEmpty) {
      // choose one of the possible moves randomly
      val n = rand(moves.size)
      mod.doMove(moves(n))
      moves = mod.getAllMoves
    }
    minTiles = Math.min(minTiles, mod.getAllTiles().size)
    println ("Min. tiles: " + minTiles)
  }

}