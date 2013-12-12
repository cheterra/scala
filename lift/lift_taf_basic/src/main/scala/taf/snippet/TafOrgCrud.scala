package taf 
package snippet 

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import java.util.Date
import taf.lib._
import taf.db._
import Helpers._
import net.liftweb.mapper._


object TafOrgCrud extends TafTAdminOrg  with LongKeyedMetaMapper[TafTAdminOrg]
    with CRUDify[Long,TafTAdminOrg] {
  // disable delete functionality
  override def deleteMenuLoc = Empty
}