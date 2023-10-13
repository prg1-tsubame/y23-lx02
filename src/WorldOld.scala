package obsolete.world

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Success, Failure}
import ExecutionContext.Implicits.global

val TICK_MS = 100
val N_TICKS = 70
val N_EXCEPTION = 80

def doPrint(num: Int, p: Promise[String]): Future[Int] = {
  val f = Future {
    /* ForkJoinPool ではブロックするスレッド数がプロセッサ数を越えると破綻する。
      * ただし、blocking 節で囲った領域については、この制限が及ばない。
      * 本例題ではブロックするスレッド数は高々2なのだが、念のため blocking 節を利用している。
      */
    blocking { Thread.sleep(TICK_MS) }
    if num % 10 == 0 then println(num)
    if num == N_EXCEPTION then p.failure(RuntimeException("some error"))
    if num > N_TICKS then p.success("Loop done.")
    num + 1
  }
  try {
    for v <- f do doPrint(v, p)
  } catch {
    case e: Exception => e.printStackTrace()
  }
  f
}

@main
def main = {
  val p = Promise[String]()
  val f = Future { doPrint(0, p) }

  p.future.onComplete {
    case Success(v) => println(v)
    case Failure(e) => e.printStackTrace()
  }

  Await.ready(awaitable = p.future, atMost = Duration.Inf)
}

class DoomsDay(message: String) extends RuntimeException(message)

class World[S](tick_ms: Int) {
  def tick(state: S): S = state

  def doomsday(message: String) = {
    throw DoomsDay(message)
  }

  def driver(state: S): Future[S] = {
    val f = Future {
      blocking { Thread.sleep(tick_ms) }
      tick(state)
    }

    try { for state <- f do driver(state) }
    catch {
        case e: DoomsDay => { println("End of the world."); throw e }
        case e: Exception => { e.printStackTrace(); throw e }
      }
    f
  }

  def init(state: S) = {
    val p = Promise[S]()
    val f = Future { driver(state) }
    p.future.onComplete {
      case Success(v) => println(v)
      case Failure(e) => { e.printStackTrace(); throw e }
    }
    Await.ready(awaitable = p.future, atMost = Duration.Inf)
  }
}

class MyWorld(tick_ms: Int) extends World[(Int, Int)](tick_ms) {
  override def tick(state: (Int, Int)) = {
    val (n, limit) = state
    val x = n + 1
    println(x)
    if x >= limit then {  // 実行の停止
      println("The end of MyWorld")
      throw new DoomsDay("The end of MyWorld")
    } else (x, limit)
  }
}

@main
def driver_test = {
  MyWorld(1000).init((0, 5))
}