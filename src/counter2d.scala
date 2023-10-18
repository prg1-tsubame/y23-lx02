package prg1.lx02.counter2d

import prg1.support.world.World

import scala.swing.event._

import java.awt.{Color, Font, Graphics2D}

object Counter {
  private val ARIAL120B = new Font("Arial", Font.BOLD, 120)
}

case class Counter(n: Int, x: Int, y: Int, tick_ms: Int) extends World(tick_ms) {

  override def tick() = {
    if (n >= 10) throw new Exception("Some error")
    Counter(n + 1, x, y, tick_ms)
  }

  override def draw(g: Graphics2D) = {
    g.setColor(new Color(0, 0, 0))
    g.setFont(Counter.ARIAL120B)
    g.drawString(s"$n", x, y)
  }

  override def keyTyped(c: Char) = {
    println(c)
    c match {
      case 'q' | 'Q' => dooms_day("Bye")
      case 'h' => Counter(n, x - 10, y, tick_ms)
      case 'l' => Counter(n, x + 10, y, tick_ms)
      case 'j' => Counter(n, x, y + 10, tick_ms)
      case 'k' => Counter(n, x, y - 10, tick_ms)
      case _   => this
    }
  }
 
  override def mouseClicked(e: MouseClicked): World = {
    Counter(n, e.point.x, e.point.y, tick_ms)
  }
}

@main def run = World.bigbang2d(new Counter(0, 500, 400, 1000), "Animate World", 1024, 800)