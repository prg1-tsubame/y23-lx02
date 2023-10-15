package prg1.graphics.color2

import java.awt.Color => JColor

case class Color(c: java.awt.Color) {}

object Color {
    def RGB(r: Int, g: Int, b: Int)          = JColor(r, g, b)
    def RGBA(r: Int, g: Int, b: Int, a: Int) = JColor(r, g, b, a)
    def HSB(h: Float, s: Float, b: Float)    = JColor.getHSBColor(h, s, b)
}

enum Colors2(val c: JColor) {
    case Black     extends Colors2(JColor.BLACK)
    case Red       extends Colors2(JColor.RED)
    case Green     extends Colors2(JColor.GREEN)
    case Blue      extends Colors2(JColor.BLUE)
    case None      extends Colors2(Color.RGBA(0, 0, 0, 0))
}

@main
def test = {
    import Colors2._

    println(Black.c)
    println(Blue.c)
}