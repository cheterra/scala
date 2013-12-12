package portal.lib

import net.liftweb.common._

object MenuChecker {
  val log = Logger("portal.lib.MenuChecker")
  
  def check(menu: String) = {
    log.debug("checking menu " + menu)
    menu match {
      case "portal_search" => true
    }
  }
  
}
