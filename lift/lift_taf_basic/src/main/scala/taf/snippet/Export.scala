package taf 
package snippet 

import common.TextToClipboard

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import java.util.Date
import taf.lib._
import taf.model._
import Helpers._

import taf.db._
import net.liftweb._

import mapper._
import util._
import http._


class Export {

    // replacing exportText and exportCsv
    def export(html: NodeSeq) : NodeSeq = {
      val toExp = S.param("toExport")
      val expType = S.param("expType")
      println("toExport: " + toExp + ", expType: " + expType)
      expType match { 
        case Full("clip") => copyToClipboard(ExportStore.makeTextList(toExp))
        case Full("file") => writeToFile    (ExportStore.makeCsvList (toExp))
        case _ => return Text("Export failed. You have not mentioned how to export (use: 'expType=xyz'): Yours: " + expType)
      }
      toExp match { 
        case Full(myS) => Text("Export '" + myS + "' as " + (expType openOr " ? ") + " done") 
        case _ => Text("Export failed. You have not mentioned what to export (use: 'toExport=xyz'): Yours: " + toExp)
      }
    }

    def copyToClipboard(text: String): Unit = {
        val t2c = new TextToClipboard();
        t2c.setText(text);
    }

    def writeToFile(text: String): Unit = {
       // http://blog.getintheloop.eu/2009/03/
       println("writeToFile -->")
       var data: Array[Byte] = text.getBytes() // get your data here
       val headers =
         ("Content-type" -> "application/csv") ::
         ("Content-length" -> data.length.toString) ::
         ("Content-disposition" -> "attachment; filname=export.csv") ::
         Nil
       println("writeToFile and stream <--")
       val response = StreamingResponse(
         new java.io.ByteArrayInputStream(data),
         () => {},
         data.length, 
         headers, Nil, 200)

       // The ResponseShortcutException will make Lift shortcut (doh) and return
       // the file instead of continuing whatever it was doing to render the page.
       throw ResponseShortcutException.shortcutResponse(response)
    }

}
