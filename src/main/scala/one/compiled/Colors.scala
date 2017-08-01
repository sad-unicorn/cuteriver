package one.compiled

object Colors {
  case class RGB(r: Int, g: Int, b: Int)
  case class HSL(h: Int, s: Int, l: Int)

  def rgb2hsl(rgb: RGB) : Either[Throwable, HSL] = {

    if (rgb.r > 255 || rgb.g > 255 || rgb.b > 255) {
      return Left(new IllegalArgumentException("r/g/b components must be less than or equal to 255"))
    }

    if (rgb.r < 0 || rgb.g < 0 || rgb.b < 0) {
      return Left(new IllegalArgumentException("r/g/b components mustn't be negative"))
    }

    val r = rgb.r.toDouble / 255
    val g = rgb.g.toDouble / 255
    val b = rgb.b.toDouble / 255

    sealed trait C {
      def value(): Double
    }
    abstract class A(c: Double) extends C {
      override def value(): Double = c
    }
    case class R(c: Double) extends A(c: Double)
    case class G(c: Double) extends A(c: Double)
    case class B(c: Double) extends A(c: Double)

    def max(a: A, b: A): A = if (a.value() > b.value()) a else b

    def min(a: A, b: A): A = if (a.value() < b.value()) a else b

    val cmax = max(R(r), max(G(g), B(b)))
    val cmin = min(R(r), min(G(g), B(b)))

    val delta = cmax.value() - cmin.value()

    var h = if (delta == 0) {
      0
    } else {
      cmax match {
        case R(_) => ((g - b) /  delta) % 6
        case G(_) => ((b - r) / delta) + 2
        case B(_) => ((r - g) / delta) + 4
      }
    } * 60

    if (h < 0) h += 360

    val l = (cmax.value + cmin.value) / 2

    val s = if (delta == 0) {
      0
    } else {
      delta / (1 - Math.abs((2 * l) - 1))
    }

    Right(HSL(Math.floor(h).toInt,Math.floor(s * 100).toInt,Math.floor(l * 100).toInt))
  }
}
