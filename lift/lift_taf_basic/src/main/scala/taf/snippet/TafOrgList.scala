package taf 
package snippet 

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import java.util.Date
import taf.lib._
import Helpers._
import java.text.SimpleDateFormat

import taf.db._
import net.liftweb._

import util._
import http._


class TafOrgList {

    def list(html: NodeSeq) : NodeSeq = {
        toShow.flatMap(org =>
            bind("item", html,
                 "name" -> org.orgName,
                 "comment" -> org.commentary ,
                 FuncAttrBindParam("view_href", {html: NodeSeq =>
                   Text("view/"+ (org.primaryKeyField))},"href"),
                 FuncAttrBindParam("edit_href", {html: NodeSeq  =>
                   Text("edit/"+ (org.primaryKeyField))},"href"),
                 FuncAttrBindParam("delete_href", {html: NodeSeq  =>
                    Text("delete/"+ (org.primaryKeyField))},"href") 
            )
        )
    }

    private def toShow =
        TafTAdminOrg.findAll();

}
