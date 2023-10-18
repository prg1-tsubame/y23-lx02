package prg1.support.world

import java.awt.{Color, Graphics2D}

import scala.swing.Swing._
import scala.swing.event._
import scala.swing.{Frame, Panel}

abstract class World(tick_ms: Int) {
  def _tick_ms: Int = tick_ms

  protected def draw(g: Graphics2D): Unit = ()

  protected def tick():            World = World.currentWorld
  protected def keyTyped(c: Char): World = World.currentWorld

  private def defaultMouseAction[T <: MouseEvent](message: String, e: T): World = {
    println(s"$message: ${e.point}")
    World.currentWorld
  }

  protected def mouseClicked(e: MouseClicked): World = defaultMouseAction("Mouse clicked", e)
  protected def mousePressed(e: MousePressed): World = defaultMouseAction("Mouse pressed", e)
  protected def mouseDragged(e: MouseDragged): World = defaultMouseAction("Mouse dragged", e)
  protected def MouseReleased(e: MouseReleased): World = defaultMouseAction("Mouse released", e)

  def dooms_day(message: String): World = {
    World.DoomsDay
  }
}

class DoomsDay extends World(0) {}
class SimpleWorld extends World(1000) {}

object World {
  val debug = false

  val DoomsDay: DoomsDay = new DoomsDay
  var currentWorld: World = new SimpleWorld
  var currentThread: Thread = Thread.currentThread

  var hasCanvas = false

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

      currentWorld.draw(g)
    }

    def handle(handle_event: () => World): Unit = {
      currentWorld = handle_event()
      currentThread.interrupt()
    }

    reactions += {
      case e: MouseClicked => handle(() => currentWorld.mouseClicked(e))
      case e: MousePressed => handle(() => currentWorld.mousePressed(e))
      case e: MouseDragged => handle(() => currentWorld.mouseDragged(e))
      case e: MouseReleased => handle(() => currentWorld.MouseReleased(e))
      case KeyTyped(_, c, _, _) => handle(() => currentWorld.keyTyped(c))
      case _: FocusLost => repaint()
    }
  }

  private def driver(world: World): Unit = {
    currentThread = Thread.currentThread
    currentWorld = world
    while (true) {
      var interrupted = false
      if (debug) println(currentWorld)
      try {
        Thread.sleep(currentWorld._tick_ms)
      } catch { case e: InterruptedException => { interrupted = true } }
      try {
        if (!interrupted) currentWorld = currentWorld.tick()
        if (currentWorld == DoomsDay) {
          println("End of the world: Doom's day has arrived.")
          scala.sys.exit()
        }
        if (hasCanvas) canvas.repaint()
      } catch {
        case e => {
          e.printStackTrace()
          scala.sys.exit()
        }
      }
    }
  }

  def bigbang(world: World): Unit = {
    driver(world)
  }

  def bigbang2d(world: World, window_title: String, width: Int, height: Int): Unit = {

    canvas.preferredSize = (width, height)
    hasCanvas = true

    val frame = new Frame {
      title = window_title
      contents = canvas
      centerOnScreen()
      open()
    }

    bigbang(world)
  }

  @main def run = bigbang(new SimpleWorld)
}