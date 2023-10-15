package prg1.graphics.color

import java.awt.Color => JColor

case class Color(c: java.awt.Color) {}

object Color {
    def RGB(r: Int, g: Int, b: Int) = Color(JColor(r, g, b))
    def RGBA(r: Int, g: Int, b: Int, a: Int) = Color(JColor(r, g, b, a))
    def HSB(h: Float, s: Float, b: Float) = Color(JColor.getHSBColor(h, s, b))
}