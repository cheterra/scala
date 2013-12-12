package taf
package snippet

import net.liftweb._
import http._
import mapper._
import taf.db._

/**
 * Declare the fields on the screen
 */
object ScreenSelectHelpdeskOffice extends LiftScreen {
  // this is also a StatefullSnippet. Surprise!
  // here are the fields and default values
  val office =    field(S.?("office"), "FRALT2212")

  def finish() {

    // Pass the state around in a RequestVar by setting injector functions in your page transition functions (e.g. SHtml.link, S.redirectTo, etc). We cover this technique in Section 3.11. 

    // If you are using a StatefulSnippet (Section 5.3.3), use this.redirectTo so that your snippet instance is used when the redirect is processed. (from http://exploring.liftweb.net/master/index-3.html#toc-Section-3.9)
    //S.redirectTo("/taf_t_fee_grp/list")
    S.redirectTo("/taf/helpdesk/screenHelpdeskOffice", () => office)

    // use the StatefulSnippet.link method (instead of SHtml.link)

    // Remember to call unregisterThisSnippet() when you are finished with your workflow
  }

}


