package prg1.lx02.automaton

import scala.sys.exit
import prg1.support.world.World

type Input = List[Char]
type State = String
type Transition = (State, Char) => State
type Accept = List[State]

case class DFA(input: Input, q: State, tick_ms: Int) extends World(tick_ms) {
    def acceptState(q: State): Boolean = {
        accept.indexOf(q) >= 0
    }

    def decorate(q: State): String = {
        q + (if (acceptState(q)) "*" else "")
    }

    override def tick(): DFA = {
        input match {
        case Nil => {
            if (acceptState(q)) { println("The input accepted.")}
            else { println("The input rejected.") }
            exit()
        }
        case c::input => {
            val q2 = transition(q, c)
            println(s"${decorate(q)}, [${c}]${input.mkString} => ${decorate(q2)}")
            DFA(input, q2, tick_ms)
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
}

@main def run = World.bigbang(DFA("1100101".toList, "q", 1000))