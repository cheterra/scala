package solitaire

import scala.swing._
import java.util.Date
import java.awt.{Color, Graphics2D }
import java.awt.event.{ActionListener, ActionEvent}

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO 

/** The field where we play */
class Board(middleground:Color = Color.lightGray) extends Component {

  val pointsX = List(79, 124, 170, 217, 262, 309, 355)
  val pointsY = List(78, 123, 170, 215, 260, 306, 352)


  val imgField: BufferedImage = ImageIO.read(new File("src/main/resources/field.png"))
  val imgHole:  BufferedImage = ImageIO.read(new File("src/main/resources/hole2.png"))

  //val mod: Model = new Model()
  val ctr: Controller = new Controller()

  preferredSize = new Dimension(500, 500)

  new javax.swing.Timer(1000, new ActionListener {
      def actionPerformed(ae:ActionEvent) { repaint }
    }).start

  override def paintComponent(g: Graphics2D) {

    /** drawing a 'hole' if there is one */
    def setTile(x: Int, y: Int) {
      if (ctr.mod.isHole(x, y))
        g.drawImage(imgHole, pointsX(x), pointsY(y), null)
    }
    
    // initially all holes are filled
    g.drawImage(imgField, 0, 0, null)

    // and we just draw the holes now
    (0 to 6).foreach(x => (0 to 6).foreach(y => setTile(x, y)))
    // move
    ctr.doAllPossibleMoves
  }

}

object Board extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Solitaire"
   try { 
     contents = new Board()
   } catch {
     case e: Exception => e.printStackTrace()
   }
  }

}
