package taf.db

import net.liftweb.mapper.{ConnectionIdentifier, ConnectionManager, Schemifier}
import java.sql.{Connection, DriverManager}
import net.liftweb.common.{Box, Empty, Full}
import net.liftweb.util.Props

object myDBVendor extends ConnectionManager {
  private var pool: List[Connection] = Nil
  private var poolSize = 0
  private val maxPoolSize = 4
 
  private lazy val chooseDriver = Props.mode match {
    case Props.RunModes.Production => "org.apache.derby.jdbc.EmbeddedDriver"
    case _ => "org.h2.Driver"
  }
  
  private lazy val chooseURL = Props.mode match {
    case Props.RunModes.Production => "jdbc:derby:lift_mapperexample;create=true"
    case _ => "jdbc:h2:mem:lift_mapperexample;DB_CLOSE_DELAY=-1"
  }

  private def createOne: Box[Connection] = try {
    val driverName: String = Props.get("db.driver") openOr chooseDriver
    val dbUrl: String = Props.get("db.url") openOr chooseURL
 
    Class.forName(driverName)
 
    val dm = (Props.get("db.user"), Props.get("db.password")) match {
      case (Full(user), Full(pwd)) =>
        DriverManager.getConnection(dbUrl, user, pwd)
 
      case _ => DriverManager.getConnection(dbUrl)
    }
 
    Full(dm)
  } catch {
    case e: Exception => e.printStackTrace; Empty
  }
 
  def newConnection(name: ConnectionIdentifier): Box[Connection] =
    synchronized {
      pool match {
        case Nil if poolSize < maxPoolSize =>
          val ret = createOne
          poolSize = poolSize + 1
          ret.foreach(c => pool = c :: pool)
          ret
 
        case Nil => wait(1000L); newConnection(name)
        case x :: xs => try {
          x.setAutoCommit(false)
          Full(x)
        } catch {
          case e => try {
            pool = xs
            poolSize = poolSize - 1
            x.close
            newConnection(name)
          } catch {
            case e => newConnection(name)
          }
        }
      }
    }
 
  def releaseConnection(conn: Connection): Unit = synchronized {
    pool = conn :: pool
    notify
  }
  
}
