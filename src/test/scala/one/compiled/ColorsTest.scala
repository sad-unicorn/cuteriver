package one.compiled

import one.compiled.Colors.{HSL, RGB}
import org.scalatest._

class ColorsTest extends FlatSpec with Matchers {
  //  RGB to HSL color conversion
  // 	Black	#000000	(0,0,0)	(0°,0%,0%)
  "black" should "produce right hsl" in {
    Colors.rgb2hsl(RGB(0,0,0)) shouldBe Right(HSL(0,0,0))
  }
  // 	White	#FFFFFF	(255,255,255)	(0°,0%,100%)
  "white" should "produce right hsl" in {
    Colors.rgb2hsl(RGB(255,255,255)) shouldBe Right(HSL(0,0,100))
  }
  // 	Red	#FF0000	(255,0,0)	(0°,100%,50%)
  "red" should "produce right hsl" in {
    Colors.rgb2hsl(RGB(255,0,0)) shouldBe Right(HSL(0,100,50))
  }
  // 	Lime	#00FF00	(0,255,0)	(120°,100%,50%)
  "lime" should "produce right hsl" in {
    Colors.rgb2hsl(RGB(0,255,0)) shouldBe Right(HSL(120,100,50))
  }
  // 	Blue	#0000FF	(0,0,255)	(240°,100%,50%)
  "blue" should "produce right hsl" in {
    Colors.rgb2hsl(RGB(0,0,255)) shouldBe Right(HSL(240,100,50))
  }
  // 	Yellow	#FFFF00	(255,255,0)	(60°,100%,50%)
  "yellow" should "produce right hsl" in {
    Colors.rgb2hsl(RGB(255,255,0)) shouldBe Right(HSL(60,100,50))
  }
  // 	Cyan	#00FFFF	(0,255,255)	(180°,100%,50%)
  "cyan" should "produce right hsl" in {
    Colors.rgb2hsl(RGB(0,255,255)) shouldBe Right(HSL(180,100,50))
  }
  // 	Magenta	#FF00FF	(255,0,255)	(300°,100%,50%)
  "magenta" should "produce right hsl" in {
    Colors.rgb2hsl(RGB(255,0,255)) shouldBe Right(HSL(300,100,50))
  }
  // 	Silver	#C0C0C0	(192,192,192)	(0°,0%,75%)
  "silver" should "produce right hsl" in {
    Colors.rgb2hsl(RGB(192,192,192)) shouldBe Right(HSL(0,0,75))
  }
  // 	Gray	#808080	(128,128,128)	(0°,0%,50%)
  "gray" should "produce right hsl" in {
    Colors.rgb2hsl(RGB(128,128,128)) shouldBe Right(HSL(0,0,50))
  }
  // 	Maroon	#800000	(128,0,0)	(0°,100%,25%)
  "maroon" should "produce right hsl" in {
    Colors.rgb2hsl(RGB(128,0,0)) shouldBe Right(HSL(0,100,25))
  }
  // 	Olive	#808000	(128,128,0)	(60°,100%,25%)
  "olive" should "produce right hsl" in {
    Colors.rgb2hsl(RGB(128,128,0)) shouldBe Right(HSL(60,100,25))
  }
  // 	Green	#008000	(0,128,0)	(120°,100%,25%)
  "green" should "produce right hsl" in {
    Colors.rgb2hsl(RGB(0,128,0)) shouldBe Right(HSL(120,100,25))
  }
  // 	Purple	#800080	(128,0,128)	(300°,100%,25%)
  "purple" should "produce right hsl" in {
    Colors.rgb2hsl(RGB(128,0,128)) shouldBe Right(HSL(300,100,25))
  }
  // 	Teal	#008080	(0,128,128)	(180°,100%,25%)
  "teal" should "produce right hsl" in {
    Colors.rgb2hsl(RGB(0,128,128)) shouldBe Right(HSL(180,100,25))
  }
  // 	Navy	#000080	(0,0,128)	(240°,100%,25%)
  "navy" should "produce right hsl" in {
    Colors.rgb2hsl(RGB(0,0,128)) shouldBe Right(HSL(240,100,25))
  }
}
