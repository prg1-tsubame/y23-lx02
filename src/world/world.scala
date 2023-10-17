package prg1.support.world

import java.awt.{Color, Font, Graphics, Graphics2D, Point}

import scala.concurrent._
import scala.concurrent.duration._
import scala.sys.exit
import scala.util.{Success, Failure}
import ExecutionContext.Implicits.global

// Scaladoc for scala-swing_3
// https://javadoc.io/doc/org.scala-lang.modules/scala-swing_3/latest/index.html
import scala.swing.Swing._
import scala.swing.event._
import scala.swing.{Frame, Panel}

object World {
  var debug = false
}

class DoomsDay(message: String) extends RuntimeException(message)
class WorldRuntimeException() extends RuntimeException()

abstract class World(tick_ms: Int) {
  def dooms_day(message: String) = { throw new DoomsDay(message) }

  protected def tick(): World

  private def driver(world: World, p: Promise[World]): Future[World] = {
    val f = Future {
      blocking { Thread.sleep(tick_ms) }
      try {
        world.tick()
      } catch {
        case e: Exception => {
          e match {
            case _: DoomsDay => { println(e.getMessage()) }
            case _ => {
              e.printStackTrace()
              Console.err.println("Exception.")
            }
          }
        }
        scala.sys.exit()
        world
      }
    }
    for world: World <- f do { driver(world, p) }
    f
  }

  def bigbang() = {
    if (World.debug) { println("bigbang") }
    val p = Promise[World]()
    val f = Future { driver(this, p) }
    p.future.onComplete {
      case Success(world) => { println(s"World: $world") }
      case Failure(e) => { Console.err.println("Faltal bug: Failure should not be raised here.") }
    }
    Await.ready(awaitable=p.future, atMost=Duration.Inf)
  }
}

object World2D {
  export World._

  var world: Option[World2D] = None

  def connect(f : World2D => Unit): Unit = {
    world match {
      case Some(w) => f(w)
      case _ => ()
    }
  }

  val canvas: Panel = new Panel {
    background = new Color(255, 255, 255)
    preferredSize = (100, 100)
    focusable = true
    listenTo(mouse.clicks, mouse.moves, keys)

    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)

      val w = size.width.toInt
      val h = size.height.toInt
      g.setColor(new Color(255, 255, 255))
      g.fillRect(0, 0, w, h)

      connect((w: World2D) => w.draw(g))
    }

    reactions += {
      case e: MousePressed => println("mouse pressed")
      case e: MouseDragged => println("mouse dragged")
      case e: MouseReleased => println("mouse released")
      case KeyTyped(_, c, _, _) => connect((w: World2D) => w.keyTyped(c))
      case _: FocusLost => repaint()
    }
  }
}


abstract class World2D(tick_ms: Int) extends World(tick_ms) {
  export java.awt.{Color, Font, Graphics2D}

  World2D.world = Some(this)
  World2D.canvas.repaint()

  def draw(g: Graphics2D): Unit
  def keyTyped(c: Char) = ()

  def bigbang(window_title: String, width: Int, height: Int) = {
    println(window_title)
    World2D.canvas.preferredSize = (width, height)

    val frame = new Frame {
      title = window_title
      contents = World2D.canvas
      pack()
      centerOnScreen()
      open()
    }
    super.bigbang()   // Start timer
  }
}