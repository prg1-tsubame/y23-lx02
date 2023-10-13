package prg1.support.world

import scala.sys.exit

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Success, Failure}
import ExecutionContext.Implicits.global

class DoomsDay(message: String) extends RuntimeException(message)

/**
  * Todo
  *  - tick : State => State に変更したい
  *  - driver : (State, Promise[State]) => Future[State] に変更したい
  **/

abstract class World[State](_state: State, tick_ms: Int) {
  val state = _state
  def dooms_day(message: String) = { throw new DoomsDay(message) }

  def tick(state: State): World[State]
  def draw(): Unit
  def keyEvent(key: String): World[State]

  // todo: driver(state, p) に変更したい
  def driver(world: World[State], p: Promise[State]): Future[World[State]] = {
    val f = Future {
      blocking { Thread.sleep(tick_ms) }
      val w = tick(world.state)  // val state = _state の参照が決め手
      w
    }
    try {
      for world: World[State] <- f do {
        println(s"World: $world")
        driver(world, p)
      }
    } catch { case e: Exception => e.printStackTrace() }
    f
  }

  def bigbang() = {
    println("bigbang")
    val p = Promise[State]()
    val f = Future{ driver(this, p) }
    p.future.onComplete {
      case Success(state) => { println(s"State: $state") }
      case Failure(e) => { e.printStackTrace(); throw e }
    }
    Await.ready(awaitable=p.future, atMost=Duration.Inf)
  }
}

type IWState = Int
class IncrementWorld(state: IWState, tick_ms: Int) extends World[IWState](state, tick_ms) {
  override def tick(state: IWState) = {
    IncrementWorld(state + 1, tick_ms)
  }
  def draw() = ()
  def keyEvent(key: String) = IncrementWorld(state, tick_ms)

  override def toString() = s"IncrementWorld($state)"
}

@main
def world_test = {
  type State = Int
  val world = IncrementWorld(0, 1000)
  world.bigbang()
}

object DFADemoObject {
  type Inputs = List[Char]
  type DFA_State = String
  type Transition = (DFA_State, Char) => DFA_State
  type Accept = List[DFA_State]
  type DFAW_State = (Inputs, DFA_State, Transition, Accept)

  class DFA_World(state: DFAW_State, tick_ms: Int) extends World[DFAW_State](state, tick_ms) {

    def acceptState(q: DFA_State, accept: Accept): Boolean = {
      accept.indexOf(state) >= 0
    }

    def decorate(q: DFA_State, accept: Accept): String = {
      q + (if (acceptState(q, accept)) "*" else "")
    }

    override def tick(state: DFAW_State): DFA_World = {
      state match {
        case (Nil, q, _, accept) => {
          if (acceptState(q, accept)) { println("The input accepted.")}
          else { println("The input rejected.") }
          exit()
        }
        case (c::inputs, q, transition, accept) => {
          val q2 = transition(q, c)
          println(s"${decorate(q, accept)}, [${c}]${inputs.mkString} => ${decorate(q2, accept)}")
          DFA_World((inputs, q2, transition, accept), tick_ms)
        }
      }
    }

    def draw() = ()
    def keyEvent(key: String) = DFA_World(state, tick_ms)
  }

  def transition(state: DFA_State, input: Char): String =
    (state, input) match {
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

  val state = ("1100101".toList, "q", transition, accept)

  @main
  def dfa_main() = {
    DFA_World(("1100101".toList, "q", transition, accept), 1000).bigbang()
  }
}
