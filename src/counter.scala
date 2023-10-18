package prg1.lx02.counter

import prg1.support.world.World

case class Counter(n: Int, tick_ms: Int) extends World(tick_ms) {
  override def tick() = {
    if (n == 5) throw new Exception("Some error")
    if (n == 4) dooms_day("End of the world")
    else {
      println(n)
      Counter(n + 1, tick_ms)
    }
  }
}

@main def run = World.bigbang(Counter(0, 1000))