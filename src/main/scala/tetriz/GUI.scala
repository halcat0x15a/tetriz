package tetriz

import javafx.application.{ Application, Platform }
import javafx.scene.{ Scene, Group }
import javafx.scene.canvas.{ Canvas, GraphicsContext }
import javafx.scene.input.{ KeyEvent, KeyCode }
import javafx.event.EventHandler
import javafx.scene.paint.Color

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import scalaz.std.anyVal._
import scalaz.std.tuple._
import scalaz.syntax.monoid._

class GUI extends Application {

  var tetriz = Tetriz()

  def draw(g: GraphicsContext, x: Int, y: Int) = {
    val width = GUI.width / Field.width
    val height = GUI.height / Field.height
    g.fillRect(x * width, y * height, width, height)
  }

  def paint(g: GraphicsContext) = {
    g.clearRect(0, 0, GUI.width, GUI.height)
    for (point <- tetriz.piece) {
      val p = point |+| tetriz.position
      draw(g, Point.x.get(p), Point.y.get(p))
    }
    for ((line, y) <- tetriz.field.zipWithIndex)
      for ((bit, x) <- line.zipWithIndex if bit)
        draw(g, x, y)
  }

  def start(stage: javafx.stage.Stage) = {
    val root = new Group
    val canvas = new Canvas(GUI.width, GUI.height)
    val g = canvas.getGraphicsContext2D
    root.getChildren.add(canvas)
    val scene = new Scene(root)
    scene.setOnKeyPressed(new EventHandler[KeyEvent] {
      def handle(key: KeyEvent) = {
        key.getCode match {
          case KeyCode.H | KeyCode.LEFT => tetriz = Tetriz.run(-1, 0, 0).exec(tetriz)
          case KeyCode.L | KeyCode.RIGHT => tetriz = Tetriz.run(1, 0, 0).exec(tetriz)
          case KeyCode.J | KeyCode.DOWN => tetriz = Tetriz.run(0, 1, 0).exec(tetriz)
          case KeyCode.K | KeyCode.UP => tetriz = Tetriz.run(0, 0, 1).exec(tetriz)
          case _ =>
        }
        paint(g)
      }
    })
    stage.setScene(scene)
    stage.show
    Future {
      while (true) {
        tetriz = Tetriz.run(0, 1, 0).exec(tetriz)
        Platform.runLater(new Runnable {
          def run = paint(g)
        })
        Thread.sleep(1000)
      }
    }
  }

}

object GUI {

  def width = 300
  def height = 600

  def main(args: Array[String]) =
    Application.launch(classOf[GUI], args: _*)

}
