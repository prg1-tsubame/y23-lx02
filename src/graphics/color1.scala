package prg1.graphics.color1

import java.awt.Color => JColor

/**
 * import prg1.graphics.color.Color._ すれば Black, Blue, ..., Transparent が利用できる
 * Transparent は透明を表す
 * Blue.c により java.awt.Color を取得できる。例: Cyan.c => java.awt.Color[r=0,g=255,b=255]
 **/

enum Color1(val c: JColor) {
    case Black extends Color1(JColor.BLACK)
    case Blue extends Color1(JColor.BLUE)
    case Cyan extends Color1(JColor.CYAN)
    case Dark_gray extends Color1(JColor.DARK_GRAY)
    case Gray extends Color1(JColor.GRAY)
    case Green extends Color1(JColor.GREEN)
    case Light_gray extends Color1(JColor.LIGHT_GRAY)
    case Magenta extends Color1(JColor.MAGENTA)
    case Orange extends Color1(JColor.ORANGE)
    case Pink extends Color1(JColor.PINK)
    case Red extends Color1(JColor.RED)
    case White extends Color1(JColor.WHITE)
    case Yellow extends Color1(JColor.YELLOW)
    case Transparent extends Color1(JColor(0, 0, 0, 0))
}

def HSB(h: Float, s: Float, b: Float) = JColor.getHSBColor(h, s, b)