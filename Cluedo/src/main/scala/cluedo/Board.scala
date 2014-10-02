package cluedo

import scala.swing._
import java.util.Date
import java.awt.{Color, Graphics2D }
import java.awt.event.{ActionListener, ActionEvent}

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO 

/** The field where we play */
class Board(middleground:Color = Color.lightGray) extends Component {

  preferredSize = new Dimension(500, 500)

  new javax.swing.Timer(1000, new ActionListener {
      def actionPerformed(ae:ActionEvent) { repaint }
    }).start

}

object Board extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Cluedo"
    try {
      //contents = new Board()
      val setup = new Setup();
      val round = setup.run
      // solver
      val solver = new cluedo.solver.RoundListener(round);
      round.lis = solver;
      for( a <- 1 to 10) { round.run }
      // last one
      solver.solver.run

    } catch {
      case e: Exception => e.printStackTrace()
    }
  }

}
