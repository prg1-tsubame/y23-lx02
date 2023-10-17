package prg1.support.world

import scala.sys.exit

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Success, Failure}
import ExecutionContext.Implicits.global

class DoomsDay(message: String) extends RuntimeException(message)
class WorldRuntimeException() extends RuntimeException()

abstract class World(tick_ms: Int) {
  def dooms_day(message: String) = { throw new DoomsDay(message) }

  def tick(): World

  def driver(world: World, p: Promise[World]): Future[World] = {
    val f = Future {
      blocking { Thread.sleep(tick_ms) }
      try {
        val w = world.tick()
        G.world = Some(w)
        w
      } catch {
        case e: Exception => {
          e match {
            case e: DoomsDay => println(e.getMessage())
            case _ => e.printStackTrace(); Console.err.println("Terminating the World from an exception.")
          }
          scala.sys.exit()
          throw e
        }
      }
    }
    for world: World <- f do { driver(world, p) }
    f
  }

  def bigbang() = {
    println("bigbang")
    val p = Promise[World]()
    val f = Future { driver(this, p) }
    p.future.onComplete {
      case Success(world) => { println(s"World: $world") }
      case Failure(e) => { Console.err.println("Faltal bug: Failure should not be raised here.") }
    }
    Await.ready(awaitable=p.future, atMost=Duration.Inf)
  }
}

case class IncrementWorld(n: Int, tick_ms: Int) extends World(tick_ms) {
  override def tick() = {
    if (n == 3) throw new Exception("Some error")
    //if (n == 3) dooms_day("End of the world")
    println(n)
    IncrementWorld(n + 1, tick_ms)
  }
}

@main
def increment_main = {
  IncrementWorld(0, 1000).bigbang()
}

object DFADemoObject {
  type Input = List[Char]
  type State = String
  type Transition = (State, Char) => State
  type Accept = List[State]

  case class DFA_World(input: Input, q: State, trans: Transition, acc: Accept, tick_ms: Int) extends World(tick_ms) {

    def acceptState(q: State): Boolean = {
      acc.indexOf(q) >= 0
    }

    def decorate(q: State): String = {
      q + (if (acceptState(q)) "*" else "")
    }

    override def tick(): DFA_World = {
      input match {
        case Nil => {
          if (acceptState(q)) { println("The input accepted.")}
          else { println("The input rejected.") }
          exit()
        }
        case c::input => {
          val q2 = trans(q, c)
          println(s"${decorate(q)}, [${c}]${input.mkString} => ${decorate(q2)}")
          DFA_World(input, q2, trans, acc, tick_ms)
        }
      }
    }
  }

  def transition(q: State, input: Char): String =
    (q, input) match {
      case ("q", '0')   => "q0"
      case ("q", '1')   => "q"
      case ("q0", '0')  => "q00"
      case ("q0", '1')  => "q"
      case ("q00", '0') => "q00"
      case ("q00", '1') => "q001"
      case ("q001", _)  => "q001"
      case _            => println("Something is wrong."); assert(false); ""
    }
  
  val accept = List("q001")

  @main
  def dfa_main() = {
    DFA_World("1100101".toList, "q", transition, accept, 1000).bigbang()
  }
}

import java.awt.{Color, Font, Graphics, Graphics2D, Point}

// Scaladoc for scala-swing_3
// https://javadoc.io/doc/org.scala-lang.modules/scala-swing_3/latest/index.html
import scala.swing.Swing._
import scala.swing.{Frame, Panel}

object G {
  var world: Option[World] = None

  val canvas: Panel = new Panel {
    background = new Color(255, 255, 255)
    preferredSize = (100, 100)

    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)

      val w = size.width.toInt
      val h = size.height.toInt
      g.setColor(new Color(255, 255, 255))
      g.fillRect(0, 0, w, h)

      world match {
        case Some(w: World2D) => w.draw(g)
        case _ => ()
      }
    }
  }
}

abstract class World2D(tick_ms: Int) extends World(tick_ms) {
  def draw(g: Graphics2D): Unit

  def bigbang(window_title: String, width: Int, height: Int) = {
    println(window_title)
    G.canvas.preferredSize = (width, height)

    val frame = new Frame {
      title = window_title
      contents = G.canvas
      pack()
      centerOnScreen()
      open()
    }
    super.bigbang()   // Start timer
  }
}

object AnimateWorld {
  val FONT = new Font("Arial", Font.BOLD, 120)
}

case class AnimateWorld(n: Int, tick_ms: Int) extends World2D(tick_ms) {
  override def tick() = {
    if (n == 10) throw new Exception("Some error")
    //if (n == 3) dooms_day("End of the world")
    println(s"world::tick ($n)")
    // println(s"canvas@${G.canvas.hashCode}")
    G.canvas.repaint()
    AnimateWorld(n + 1, tick_ms)
  }

  override def draw(g: Graphics2D) = {
    val w = G.canvas.size.width.toInt
    val h = G.canvas.size.height.toInt
    println(s"draw ($n - $w x $h)")
    g.setColor(new Color(0, 0, 0))
    g.setFont(AnimateWorld.FONT)
    g.drawString(s"$n", w / 2, h / 2)
  }
}

@main def animate = AnimateWorld(0, 1000).bigbang("Animate World", 1024, 800)