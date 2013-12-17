package demo

import scala.swing._
import java.util.Date
import java.awt.{Color, Graphics2D }
import java.awt.event.{ActionListener, ActionEvent}

class Clock(scale:Int, middleground:Color = Color.lightGray) extends Component {

  val digitData = List("-|| ||-", "  |  | ", "- |-| -", "- |- |-", " ||- | ",
                       "-| - |-", "-| -||-", "- |  | ", "-||-||-", "-||- |-")
  val digitRects = Array((1,3,3,1),(0,4,1,3),(4,4,1,3),(1,7,3,1),
                         (0,8,1,3),(4,8,1,3),(1,11,3,1))
  val digitX = List(3,9,17,23,31,37)
  val points = List((15,6),(15,9),(29,6),(29,9))

  preferredSize = new Dimension(45*scale, 15*scale)

  new javax.swing.Timer(1000, new ActionListener {
      def actionPerformed(ae:ActionEvent) { repaint }
    }).start

  override def paintComponent(g: Graphics2D) {
    def scaledRect(x:Int, y:Int, w:Int, h:Int):Unit =
      g.fillRect(x*scale, y*scale, w*scale, h*scale)
    def digit(d:Int, dx:Int):Unit = (0 to 6).foreach { k =>
      g.setColor(if (digitData(d)(k) == ' ') middleground else foreground)
      val (x,y,w,h) = digitRects(k)
      scaledRect(x + dx, y, w, h)
    }

    g.setColor(background)
    scaledRect(0,0, 45, 15)
    val date = new Date
    val (h,m,s) = (date.getHours, date.getMinutes, date.getSeconds)
    List(h/10,h%10,m/10,m%10,s/10,s%10).zip(digitX).foreach(t => digit(t._1,t._2))
    g.setColor(foreground)
    points.foreach(t => scaledRect(t._1, t._2, 1, 1))
  }
}

object Clock extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Clock"
    contents = new Clock(4)
  }

}
