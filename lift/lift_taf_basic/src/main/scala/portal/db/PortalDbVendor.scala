package portal.db

import net.liftweb.mapper.{ConnectionIdentifier, ConnectionManager, Schemifier}
import java.sql.{Connection, DriverManager}
import net.liftweb.common.{Box, Empty, Full}
import net.liftweb.util.Props
import org.apache.commons.codec.binary.Base64

object portalDB extends ConnectionIdentifier {
  def jndiName = "portal"
}


object portalDBVendor extends ConnectionManager {
  private var pool: List[Connection] = Nil
  private var poolSize = 0
  private val maxPoolSize = 4
 
  private def createOne: Box[Connection] = try {
    def prefix = "pdb."
    val driverName: String = Props.get(prefix + "driver")  openOr "no driver"
    val dbUrl:      String = Props.get(prefix + "url")     openOr "no url"

    Class.forName(driverName)

    def decode(s: String) = new String(new Base64().decode(s.getBytes()))

    var pw = Props.get(prefix + "password") openOr "no-pw"
    if (pw.startsWith("#"))
      pw = decode(pw.substring(1))
    else
      println("use " + new String(new Base64().encode(pw.getBytes())))
 
    val dm = (Props.get(prefix + "user"), Full(pw)) match {
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
