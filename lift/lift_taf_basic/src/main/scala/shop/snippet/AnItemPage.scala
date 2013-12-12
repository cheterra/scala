package shop
package snippet


import model.Item
import comet._

import net.liftweb._
import util._
import Helpers._
import common._
import http._
import sitemap._

import scala.xml.Text

object AnItemPage {
  // create a parameterized page
  def menu = Menu.param[Item]("Items", 
                              Loc.LinkText(i => Text(i.name)),
                              Item.find _, _.id) / "shop" / "an_item" / *

//  def menu2 = Menu.param[ParamInfo]("Param3", "MyParam3", 
//                                   s => Full(ParamInfo(s)), 
//                                   pi => pi.theParam) / "shop" / "param3"

  // TEST   
  lazy val menu2 = Menu.param[Item]("Param2", 
                                   Loc.LinkText(i => Text(i.name)), 
                                   Item.find _, _.id) / "shop" / "an_item"
}

class AnItemPage(item: Item) {
  def render = "@name *" #> item.name &
  "@description *" #> item.description &
  "@price *" #> item.price.toString &
  "@add_to_cart [onclick]" #> SHtml.ajaxInvoke(() => TheCart.addItem(item))
}