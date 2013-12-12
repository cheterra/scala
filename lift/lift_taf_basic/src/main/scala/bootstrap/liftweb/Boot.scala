package bootstrap.liftweb

import net.liftweb._
import util._
import Helpers._

import common.{Full,Empty,Logger}
import http._
import sitemap._
import Loc._
import mapper._

import code.model._
import code.snippet._
import code.lib._

import shop.lib._
import shop.model._
import shop.snippet._
import shop.comet._

import taf.db._
import taf.snippet._

import portal.db._
import portal.snippet._
import portal.lib.MenuChecker

import france.snippet._



/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
    if (!DB.jndiJdbcConnAvailable_?) {
      val vendor = 
	new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
			     Props.get("db.url") openOr 
			     "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
			     Props.get("db.user"), Props.get("db.password"))

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
    }

    // Use Lift's Mapper ORM to populate the database
    // you don't need to use Mapper to use Lift... use
    // any ORM you want
    //Schemifier.schemify(true, Schemifier.infoF _, User)

//    DB.defineConnectionManager(DefaultConnectionIdentifier, tafDBVendor)
    DB.defineConnectionManager(DefaultConnectionIdentifier, tafDBVendor)
    DB.defineConnectionManager(portalDB, portalDBVendor)
    // Calculate the name of a table based on the name of the Mapper [details]
    // Must be set in Boot To get snake_case, use this MapperRules.columnName = (_,name) => StringHelpers.snakify(name) 
    MapperRules.tableName = (_, name) => StringHelpers.snakify(name)
    MapperRules.columnName = (_, name) => StringHelpers.snakify(name)

    // Set up a logger to use for startup messages
    val logger = Logger(classOf[Boot])

    // Add a query logger
    DB.addLogFunc {
      case (log, duration) => {
        logger.debug("Total query time : %d ms".format(duration))
        log.allEntries.foreach {
          case DBLogEntry(stmt,duration) =>
            logger.debug("  %s in %d ms".format(stmt, duration))
        }
      }
    }

    // where to search snippet
    LiftRules.addToPackages("code")

    // where to search snippet, 22.06.2011 Ho
    LiftRules.addToPackages("taf")
    // only the root directory is searched, not the sub directories
//    LiftRules.addToPackages("taf.helpdesk")
//    LiftRules.addToPackages("taf/zombies")
//    LiftRules.addToPackages("taf/payment")
//    LiftRules.addToPackages("zombies")
//    LiftRules.addToPackages("payment")
//    LiftRules.addToPackages("taf.zombies")
//    LiftRules.addToPackages("taf.payment")
//    LiftRules.addToPackages("taf.snippet.zombies")
//    LiftRules.addToPackages("taf.snippet.payment")
    //LiftRules.addToPackages(java.lang.Package.getPackage("taf.snippet.payment.SelectedPaymentFee"))
    //LiftRules.addToPackages(java.lang.Package.getPackage("taf.snippet.zombies"))

    // where to search snippet, 25.02.2012 Ho
    LiftRules.addToPackages("france")
    // where to search snippet, 31.05.2012 Ho
    LiftRules.addToPackages("portal")
    // where to search snippet, 16.06.2012 Ho
    LiftRules.addToPackages("common")
    // where to search snippet, 22.06.2011 Ho
    LiftRules.addToPackages("chat")
    // where to search snippet, 25.06.2011 Ho
    LiftRules.addToPackages("form")
    // where to search snippet, 26.06.2011 Ho
    LiftRules.addToPackages("rest")
    // where to search snippet, 02.07.2011 Ho
    LiftRules.addToPackages("shop")

    /**
     * Calculate if the page should be displayed.
     * In this case, it will be visible every other minute
     */
    def displaySometimes_? : Boolean = 
      (millis / 1000L / 60L) % 2 == 0
      
    /**
     * Calculate if the menu is accessible for the actual user
     */
    def check(menu: String)(): Boolean = MenuChecker.check(menu)  
    

    // for TAF crudify
    //val menus = ... Menu(Loc(...)) :: Expense.menus
    //LiftRules.setSiteMap(SiteMap(menus : _*))

    // Build SiteMap
    def sitemap = SiteMap(
      Menu.i("Home") / "index" >> User.AddUserMenusAfter, // the simple way to declare a menu


      Menu.i("Sometimes") / "sometimes" >> If(displaySometimes_? _,
                                            S ? "Can't view now"),

      // A menu with submenus
      Menu.i("Info") / "info" submenus(
        Menu.i("About")           / "about" >> Hidden >> LocGroup("bottom"),
        Menu.i("Recursive/Embed") / "recursive",
        //Menu.i("Recurse")         / "recurse",
        //Menu.i("Param")           / "param",
        Menu.i("Feedback")        / "feedback" >> LocGroup("bottom")),

      // A menu with submenus
      Menu.i("Form")          / "form" / "index" submenus(
        Menu.i("Dump")        / "form" / "dump",
        Menu.i("OnSubmit")    / "form" / "onsubmit",
        Menu.i("Stateful")    / "form" / "stateful",
        Menu.i("Request Var") / "form" / "requestvar",
        Menu.i("Field Error") / "form" / "fielderror",
        Menu.i("Screen")      / "form" / "screen",
        Menu.i("Wizard")      / "form" / "wizard",
        Menu.i("Ajax")        / "form" / "ajax",
        Menu.i("Query")       / "form" / "query",
        Menu.i("Last")        / "form" / "dummy"
      ),

      Param.menu,

      Menu.param[Which]("Recurse", "Recurse",
                        {case "one" => Full(First())
                         case "two" => Full(Second())
                         case "both" => Full(Both())
                         case _ => Empty},
                        w => w.toString) / "recurse",


      // A menu with submenus
      Menu.i("Rest")          / "rest" / "index" submenus(
        Menu.i("Helper")      / "rest" / "helper",
        Menu.i("Full")        / "rest" / "full"
      ),

      // A menu with submenus
      Menu.i("Shop")              / "shop" / "index" submenus(
        AllItemsPage.menu,  // "shop/items"
        AnItemPage.menu2,
        Menu.i("All items")       / "shop" / "all_items",
        Menu.i("The cart")        / "shop" / "comet_cart",
        Menu.i("Share link")      / "shop" / "_share_link",
        Menu.i("Nothing")     / "nothing"
      ),

      // more complex because this menu allows anything in the
      // /static path to be visible
      Menu(Loc("Static", Link(List("static"), true, "/static/index"), 
	       "Static Content")),


      //Menu(Loc("TAF III", Link(List("taf"), true, "/taf/enhanced"), 
      //	       "TAF Content")),

      // A menu with submenus
      Menu.i("TAF")                 / "taf" / "index" submenus(
        Menu.i("Enhanced")          / "taf" / "enhanced",
        Menu.i("List AdminOrg")     / "taf" / "listAdminOrg",
        Menu.i("Screen AdminOrg")   / "taf" / "screenAdminOrg" submenus(
          TafOrgCrud2.menus),
        Menu.i("Manager Selects")   / "taf" / "screenSelectFeeTable" submenus(
          TafFeeGroupCrud.menus),
        Menu.i("Small Selects - Abstiegslogik")     / "taf" / "scope" / "screenSelectFeeTableSmall",
        Menu.i("Small Results")     / "taf" / "scope" / "screenResultFeeTableSmall" submenus(
          Menu(Loc("Person", Link(List("taf"), true, "/taf/pers"), "Person Content")),
          //Menu.i("Person")        / "taf" / "scope" / "pers",
          Menu.i("Office")          / "taf" / "scope" / "office" / **
        ),
        Menu.i("Abstiegslogik")     / "taf" / "scope"      / "screenSelectFeeGrpSearchCriteria",
        Menu.i("Zombie Fees")       / "taf" / "zombies"    / "screenZombieFees",
        Menu.i("Helpdesk Office")   / "taf" / "helpdesk"   / "screenSelectHelpdeskOffice",//"screenHelpdeskOffice",
        Menu.i("Payment Fee")       / "taf" / "payment"    / "screenPaymentFee",
        Menu.i("Nothing-TAF")     / "nothing-taf"
      ),

      // A menu with submenus
      Menu.i("Portal")          / "portal" / "index" submenus(
        Menu.i("Search Product")    / "portal" / "article" / "screenSearchProduct" >> If(check("portal_search") _,  S ? "Can't view now"), 
        //Menu.i("Result Product")    / "portal" / "article" / "screenProduct",
        // that works: Menu.i("Selected Product")  / "portal" / "article" / "screenSelectedProduct",
        // that works, but I do not want the customer to access it: Menu.i("Edit")              / "portal" / "article" / "edit",
        Menu(Loc("Common", Link(List("portal"), true, "/portal/common"), "Common"))
        //Menu.i("Test Export")    / "portal" / "article" / "export2",
      ),

      // A menu with submenus
      /*
      Menu.i("Francais")          / "france" / "index" submenus(
        Menu.i("Grammaire")           / "france" / "grammar_question",
        Menu.i("Grammaire Resp")      / "france" / "grammar_response",
        Menu.i("Grammaire finished")  / "france" / "grammar_finished",
        Menu.i("Lessons")             / "france" / "grammar_lessons" submenus(
          GrammarMenu.menu2),
        Menu.i("Dump2")               / "france" / "dump",
        Menu.i("OnSubmit2")           / "france" / "onsubmit",
        Menu.i("Nothing Franz")       / "nothing-france"
      ),
      */

      Menu(Loc("Chat", Link(List("chat"), true, "/chat/index"), "Chat"))
    )

    def sitemapMutators = User.sitemapMutator

    // set the sitemap.  Note if you don't want access control for
    // each page, just comment this line out.
    LiftRules.setSiteMapFunc(() => sitemapMutators(sitemap))

    // Use jQuery 1.4
    LiftRules.jsArtifacts = net.liftweb.http.js.jquery.JQuery14Artifacts

    //Show the spinny image when an Ajax call starts
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    // Make the spinny image go away when it ends
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // What is the function to test if a user is logged in?
    LiftRules.loggedInTest = Full(() => User.loggedIn_?)

    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set((r: Req) =>
      new Html5Properties(r.userAgent))    

    // Make a transaction span the whole HTTP request
    S.addAround(DB.buildLoanWrapper)

    // 26.06.2011 Ho
    // the stateless REST handlers
    LiftRules.statelessDispatchTable.append(BasicExample.findItem)
    LiftRules.statelessDispatchTable.append(BasicExample.extractFindItem)

    // stateful versions of the same
    // LiftRules.dispatch.append(BasicExample.findItem)
    // LiftRules.dispatch.append(BasicExample.extractFindItem)

    // 29.06.2011 Ho
    LiftRules.statelessDispatchTable.append(BasicWithHelper)

    // 22.07.2011 Ho  not sure if this is not already obsolete in Lift 2.3
    LiftRules.resourceNames = "resources" :: "taf/resources" :: "portal/resources" :: Nil

  }
}
