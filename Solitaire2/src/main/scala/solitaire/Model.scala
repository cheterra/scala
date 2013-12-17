package solitaire

import java.awt.Rectangle

/** a Point is a helper with x and y coordinate */
class Point (val x: Int, val y: Int) {
  def +(p: Point): Point = new Point(x + p.x, y + p.y)
  def *(n: Int): Point = new Point(x * n, y * n)
  def <(p: Point): Boolean = x < p.x || y < p.y
  def >(p: Point): Boolean = x > p.x || y > p.y
  def check: Unit = if (x < -1 || y < -1) throw new Exception("invalid: " + toString())
  override def toString  (): String = "[" + x + ", " + y + "]"
}

/** the Tile is the pawn in game, the stick in solitaire, something which can be moved */
class Tile (x: Int, y: Int)  extends Point(x, y) {
  if (x < 0) throw new IllegalArgumentException ("x must not be negative " + x)
  if (y < 0) throw new IllegalArgumentException ("y must not be negative " + y)
}

/** a move is vector a stick can do in this game */
class Move (x: Int, y: Int) extends Point(x, y)

/** Includes starting and ending position */
class RealMove(val tile: Tile, val move: Move) {
   override def toString  (): String = "[" + tile.x + ", " + tile.y + " -> " + move + "]"
}

/** Model - contains the data */
class Model {
  /** The game */
  val areaBase = List (
                   "..xxx..",
                   "..xxx..",
                   "xxxxxxx",
                   "xxxoxxx",
                   "xxxxxxx",
                   "..xxx..",
                   "..xxx..")
  val P0: Point = new Point(0, 0);
  val P6: Point = new Point(6, 6);
  
  /** to move the sticks I want them to have in array for performance reasons */
  def getArea = areaBase.map( _ toArray)

  /** the field map as array */
  var area = getArea
  def area(p: Point): Char =  { area(p.y)(p.x) }
  def setArea(p: Point, ch: Char): Unit = { area(p.y)(p.x) = ch }
  
  def reset = { area = getArea }

  /** an awt rectangle from (0,0) to (cols,rows) of the field */
  val rect  = new Rectangle(0, 0, 
      // area(0).size is the number of chars of the first line. That is the width of the field.
      area(0).size,
      // area.size is the number of string. That is the height of the field
      area.size)
  
  val up    = new Move(0,  1)
  val down  = new Move(0, -1)
  val left  = new Move(-1, 0)
  val right = new Move(1,  0)
  /** four moves: up, down, left, right */
  val moves = List(up, down, left, right)

  /** 
   * Moves are limited: 
   *  <ul>
   *    <li>the sticks cannot leave the field</li>
   *    <li>in this game the next field must be occupied by a stick
   *         and the over next field must be empty. That are the rules of solitaire.</li>
   *  </ul> 
   */
  def canMove (t: Tile, m: Move): Boolean = {
    var p2 = t + m * 2;
    if (p2 < P0) return false
    if (p2 > P6) return false

    p2.check
    
     // the first check is not really required if the precondition
     // is that this is an existing tile
    
     // the next field must be free (e.g. x) AND the over next field must be free (e.g. o)
     area(t + m) == 'x' && area(t + m * 2) == 'o'
  }

  /** move: o o x, e.g. jump over one stick and remove it */
  def doMove (t: Tile, m: Move): Unit = {
    setArea(t,         'o')
    setArea(t + m,     'o')
    setArea(t + m * 2, 'x')
  }

  def doMove (m: RealMove): Unit  = doMove(m.tile, m.move)

  /** Get all tiles marked as x */
  def getAllTiles(): IndexedSeq[Tile] = {
    // walk over all x and y and emit a new Tile
    var w = rect.width
    var h = rect.height
    for (x <- 0 until w; 
         y <- 0 until h 
         if area(y)(x) == 'x') yield new Tile(x, y)
  }

  /** Get all possible moves (up, down...) for this tile */
  def getAllMoves(t: Tile): List[RealMove] = {
    t.check
    for (m <- moves if canMove(t, m) ) yield new RealMove(t, m)
  }

  /** Get all possible moves for this model */
  def getAllMoves(): Seq[RealMove] = {
    getAllTiles().flatMap(getAllMoves(_))
  }

  /** check if there is hole in the field at x, y */
  def isHole(x: Int, y: Int): Boolean = {
    area(y)(x) == 'o'
  }

  // debug
  def show() = {
    area.foreach(showLine( _ ))
  }
  
  // debug
  def dump(moves: Seq[RealMove]) = moves.foreach ( n => println("--" + n))

  // not used
  def showLine(line: Array[Char]) = {
    line.foreach(print( _ ))
    println()
  }

}


