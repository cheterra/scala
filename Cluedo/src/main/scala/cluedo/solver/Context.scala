package cluedo.solver

import cluedo.Player
import cluedo.Card

class Context (val testees: List[Testee], val cards: List[Card]) {
  /** all players but not the ones in the list */
  def other(player: List[Player]): List[Testee] = testees.diff(player);
  def otherCards(theCards: List[Card]): List[Card] = cards.diff(theCards)
  /** convert  players into testees */
  def asTestee(player: List[Player]): List[Testee] = testees.filter(player.contains(_));
  def asTestee(player: Player): Testee = asTestee(List(player)).head
  
}

// ------------

class Tweet {}

trait TweetStream {
  def subscribe(f: Tweet => Unit)
}
class HosebirdStream extends TweetStream {
  def subscribe(f: Tweet => Unit) = println("subscribe called")
}
class FileStream extends TweetStream {
  def subscribe(f: Tweet => Unit) = { f(new Tweet); println("f called") }
}

class TweetCounter(stream: TweetStream) {
  var count: Int = 0;
  stream.subscribe { tweet => count += 1 }
  println("count: " + count)
}

// in repl: val it = new Testit
class TestIt {
  val hb: HosebirdStream = new HosebirdStream
  val tc: TweetCounter = new TweetCounter(hb);
  // subscribe called
  val t2: TweetCounter = new TweetCounter(new FileStream());
  // f called, count = 1
}

// --------------

class User(val username: String, val password: String)

trait UserRepositoryComponent {
  val userRepository = new UserRepository
  // a dummy service that is not persisting anything
  // solely prints out info
  class UserRepository {
    def authenticate(user: User): User = { 
      println("authenticating user: " + user)
      user
    }
    def create(user: User) = println("creating user: " + user)
    def delete(user: User) = println("deleting user: " + user)
  }
} 

// using self-type annotation declaring the dependencies this 
// component requires, in our case the UserRepositoryComponent
trait UserServiceComponent { 
  this: UserRepositoryComponent =>;
  val userService = new UserService  
  class UserService {
    def authenticate(username: String, password: String): User = 
      userRepository.authenticate(new User(username, password))  
    def create(username: String, password: String) = 
      userRepository.create(new User(username, password))
    def delete(user: User) = userRepository.delete(user)
  }
}

