package prg1.world.test

// 公式ドキュメントでの Futures and Promises の解説 - https://docs.scala-lang.org/overviews/core/futures.html#futures

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Success, Failure}
import ExecutionContext.Implicits.global

/**
  * World 実装の基礎実験。
  * TICK_MS ミリ秒ごとに起きて数値を表示し、数値を +1 する。
  * N_TICKS 回目に終了する。
  * THROW_EXCEPTION ならば、例外を飛ばし、そのメッセージがエラー出力に出ることを確認する。
  * そうでなければ、"Driver loop finished ..." というメッセージを標準出力に出す。
  *
  * 詳しい解説は https://github.com/prg1-tsubame/lx2023/wiki/prg1.world.test
  * 苦節の跡は lx/scala3/bigbang.scala を参照。
  */

class DoomsDay(message: String) extends RuntimeException(message)

object World {
  enum Result { case Success, Failure, Doom }
  val R = Result.Doom

  val TICK_MS = 100
  val N_TICKS = 8

  def dooms_day(message: String) = { throw new DoomsDay(message) }
  
  def next(n: Int, p: Promise[Int]): Unit = {
    if (p.isCompleted) return

    val future = Future[Int] {
      try {
        println(s"n = $n")

        if (n == N_TICKS / 2) {
          if (R == Result.Failure) { throw (new RuntimeException("Interrupted with a runtime error")) }
          if (R == Result.Doom) { dooms_day("The end of the game") }
        }
        if (n == N_TICKS) { p.success(n) }

        blocking { Thread.sleep(TICK_MS) }

        n + 1
      } catch { case e: Exception => { p.failure(e); n } }
    }

    for n <- future do next(n, p)
  }

  @main def main() = {
    val p = Promise[Int]()
    val f = Future { next(0, p) }

    p.future.onComplete {
      case Success(v) => { println(s"Driver loop finished with num = $v") }
      case Failure(e: DoomsDay) => { println(s"Doom's day: ${e.getMessage()}") }
      case Failure(e) => { e.printStackTrace() }
    }

    Await.ready(awaitable = p.future, atMost = Duration.Inf)
  }
}