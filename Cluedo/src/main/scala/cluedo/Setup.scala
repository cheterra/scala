package cluedo;

/** Setup the game, distribute the cards to the players */
class Setup {
  
  def run: Round = {
			val thePlayers = new Players(List("Anna", "Julia", "Heike", "Gisela", "Frank"))
			val theCards = new Cards();
			println("the cards: " + theCards);
			val stack = new CardMix(theCards.all);
			val talon = new Talon(stack.all)
			stack.remove(talon.all);
			println("talon: " + talon);
			distributeCards(stack.all, thePlayers);
			println("players:\n" + thePlayers)
			// who should start?
			thePlayers.selectLeader(thePlayers.leader)
			return new Round(thePlayers, theCards);
	}

  /** Give cards from the stack to each player until the stack is empty */
	def distributeCards(cards: Seq[Card], thePlayers: Players) =
	  cards.foreach( { thePlayers.next.addCard(_) } )
	  
}
