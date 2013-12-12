package taf 
package lib

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import java.util.Date
import taf.lib._
import Helpers._

import taf.db._
import net.liftweb._

import mapper._
import util._
import http._


/** Having some utils to create export links here */
object ExportUtil {
    def doFunc(html: NodeSeq, toExport: String, expType: String, theRest: String) =
                 FuncAttrBindParam(expType + "_href", {html: NodeSeq =>
                   Text("../common/export?toExport=" + toExport + "&expType=" + expType + "&expText=" + theRest)},"href")


    def doBind(html: NodeSeq, toExport: String, expType: String, theRest: String) = bind("exp", html,
                 doFunc(html, toExport, "clip", theRest),
                 doFunc(html, toExport, "file", theRest))

}