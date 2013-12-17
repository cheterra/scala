package demo

import swing._
import event._

object Converter extends SimpleSwingApplication {
  def newField = new TextField {
    text = "0"
    columns = 5
  }
  val celsius = newField
  val fahrenheit = newField
  def top = new MainFrame {
    title = "Convert Celsius / Fahrenheit"
    contents = new FlowPanel(celsius, new Label(" Celsius = "),
                             fahrenheit, new Label(" Fahrenheit"))

  }

}
