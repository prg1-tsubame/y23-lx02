package prg1.lx02.counter2d

import prg1.support.world.World2D

import java.awt.Font

object Counter {
  private val ARIAL120B = new Font("Arial", Font.BOLD, 120)
}

case class Counter(n: Int, tick_ms: Int) extends World2D(tick_ms) {

  override def tick() = {
    if (n == 10) throw new Exception("Some error")
    //if (n == 3) dooms_day("End of the world")
    
    Counter(n + 1, tick_ms)
  }

  override def draw(g: Graphics2D) = {
    val s = World2D.canvas.size
    val w = s.width.toInt
    val h = s.height.toInt
    if (World2D.debug) println(s"draw ($n - $w x $h)")
    g.setColor(new Color(0, 0, 0))
    g.setFont(Counter.ARIAL120B)
    g.drawString(s"$n", w.toInt / 2, h.toInt / 2)
  }

  override def keyTyped(c: Char) = println(c)
}

@main def run = Counter(0, 1000).bigbang("Animate World", 1024, 800)