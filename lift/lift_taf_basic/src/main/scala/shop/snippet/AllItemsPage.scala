package shop
package snippet

import model.Item
import comet._

import net.liftweb._
import http._
import sitemap._
import util._
import Helpers._
import common._

// capture the page parameter information
case class ParamInfo(theParam: String)

// a snippet that takes the page parameter information
class ShowParam(pi: ParamInfo)  {
  def render = "*" #> {pi.theParam + " ha, ha"}
}


object AllItemsPage {
  // define the menu item for the page that
  // will display all items
  // TEST "itemS"
  lazy val menu = Menu.i("Items") / "shop" / "items" >>
  // this creates a snippet named "Items" with the function "render" in shop/item
  Loc.Snippet("Items", render)

  // display the items
  def render =
    "tbody *" #> renderItems(Item.inventoryItems)

  // for a list of items, display those items
  def renderItems(in: Seq[Item]) =
    "tr" #> in.map(item => {
      "a *" #> item.name &
      "a [href]" #> AnItemPage.menu.calcHref(item) &
      "@description *" #> item.description &
      "@price *" #> item.price.toString &
      "@add_to_cart [onclick]" #>
      SHtml.ajaxInvoke(() => TheCart.addItem(item)
    )})
}
