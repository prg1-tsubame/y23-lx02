package prg1.support.world2d

abstract class World2D(tick_ms: Int) extends prg1.support.world.World(tick_ms) {
  def draw(): Unit
}

case class IncrementWorld(n: Int, tick_ms: Int) extends World2D(tick_ms) {
  override def tick() = {
    if (n == 3) throw new Exception("Some error")
    //if (n == 3) dooms_day("End of the world")
    println(n)
    IncrementWorld(n + 1, tick_ms)
  }

  override def draw() = {
    println("draw")
  }

  def bigbang(height: Int, width: Int) = {
    super.bigbang()
  }
}

@main def increment_main = IncrementWorld(0, 1000).bigbang(300, 300)