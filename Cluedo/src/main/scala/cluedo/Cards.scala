package cluedo

import scala.collection.mutable.Stack

/** Cards like 'who' */
class Card(val name: String, val cat: String) {
	override def toString() = name + " (" + cat + ")";
}

/** Cards e.g. Persons and Locations and so on */
class Cards {
  val props: CluedoProperties = new CluedoProperties("src/main/resources/hp.properties");
  lazy val who:   List[Card] = split("who");
  lazy val where: List[Card] = split("where");
  lazy val what:  List[Card] = split("what");
  lazy val all:   List[Card] = List(who, where, what).flatten 
  
  def split(cat: String): List[Card] = {
    val line: String = props.get(cat);
    return line.split(",").toList
    // trim all names
    .map( { _.trim()} )
    // create a Card for each name + cat
    .map( {new Card(_, cat) } );
  }
  
  // abusing Talon to get three random cards, one for each category
  def random3: List[Card] = new Talon(util.Random.shuffle(all)).all;
  override def toString() =    "\nwho: " + who + "\n"    + "where: " + where    + "\nwhat: " + what; 
}

/** The cards - already mixed, presented as stack */
class CardMix(val cards: List[Card]) {
  var stack: Stack[Card]= new Stack().pushAll(mix)

  private def mix: List[Card] = util.Random.shuffle(cards)
  
  def all: Stack[Card] = return stack;
  def drawOne: Card = stack.pop;
  def remove(cards: List[Card]) = { this.stack = stack.diff(cards) }
}

/** Talon - The hidden cards in the middle of the game */
class Talon(val cards: Seq[Card]) {
  val who:   Option[Card]   = find("who")
  val where: Option[Card]   = find("where")
  val what:  Option[Card]   = find("what")
  lazy val all: List[Card] = List(who, where, what).flatten;
  
  def find(category: String): Option[Card] = cards.find( _.cat.equals(category))
	override def toString() = "talon (" + all + ")";
}
