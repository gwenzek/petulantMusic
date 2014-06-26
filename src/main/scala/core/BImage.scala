package core

import com.sksamuel.scrimage.{Image, MutableImage, X11Colorlist, Color}
import java.awt.Graphics2D
import java.io.File
import impl.EasyMath.{EasyIterable, range2D}
import scala.language.implicitConversions


class BImage(override val awt: java.awt.image.BufferedImage) extends Image(awt) {

    import BImage._
    def dilate(pattern: Pattern): BImage = {
        val dilated = this.toMutable
        for(x <- pattern.width until (this.width - pattern.width);
            y <- pattern.height until (this.height- pattern.height)){
            if(pattern.anchorAt(this, x, y)) pattern.drawAt(dilated, x, y)
        }
        dilated
    }

    def erode(pattern: Pattern, background: Color = X11Colorlist.White): BImage = {
        val eroded = Image.filled(this.width, this.height, background).toMutable
        for(x <- pattern.width until (this.width - pattern.width);
            y <- pattern.height until (this.height- pattern.height)){
            if(pattern.fitAt(this, x, y)) pattern.drawAt(eroded, x, y)
        }
        eroded
    }

    def erase(pattern: Pattern, background: Int = BImage.White): BImage = {
        val erased = MutableImage(this)
        for(x <- pattern.width until (this.width - pattern.width);
            y <- pattern.height until (this.height- pattern.height)){
            if(pattern.fitAt(this, x, y)) pattern.clearAt(erased, x, y)
        }
        erased
    }

    def close(pattern: Pattern) = dilate(pattern).erode(pattern)
    def open(pattern: Pattern) = erode(pattern).dilate(pattern)

}

object BImage {
    val Black = 0xFF000000
    val White = 0xFFFFFFFF

    def binarize(img: Image): BImage = {
        def rgbToGray(r: Int, g: Int, b: Int): Int = (0.2126 * r + 0.7152 * g + 0.0722 * b).toInt
        def rgbToGrayScale(rgb: Int): Int = rgbToGray(rgb & 0xFF0000 >> 16, rgb & 0x00FF00 >> 8, rgb & 0x0000FF)
        def binarize(rgb: Int) = if(rgbToGrayScale(rgb) < 150) Black else White

        img.map( (x: Int, y: Int, rgb: Int) => binarize(rgb) )
    }

    implicit def asBImage(img: Image): BImage = new BImage(img.awt)
}

case class Pixel(x: Int, y: Int, color: Int)
class Pattern(val pixels: List[Pixel], val color: Int){

    val width = pixels.maxValue[Int]((p: Pixel) => math.abs(p.x))
    val height = pixels.maxValue[Int]((p: Pixel) => math.abs(p.y))

    def this(pixels: List[Pixel]) = this(pixels, pixels(0).color)

    def fitAt(img: Image, x0: Int, y0: Int) : Boolean = {
        pixels.forAll((p: Pixel) => img.pixel(x0+p.x, y0+p.y) == p.color)
    }

    def anchorAt(img: Image, x0: Int, y0: Int) : Boolean = img.pixel(x0, y0) == color

    def drawAt(img: MutableImage, x0: Int, y0: Int) = {
        for(p <- pixels) img.setPixel(x0 + p.x, y0 + p.y, p.color)
    }
    
    def clearAt(img: MutableImage, x0: Int, y0: Int,  background: Int = BImage.White) = {
        for(p <- pixels) img.setPixel(x0 + p.x, y0 + p.y, background)
    }
}

object Pattern {
    def hLine(length: Int, color: Int = BImage.Black): Pattern = {
        val l0 = length / 2
        val l1 = length - l0 - 1
        new Pattern((-l0 to l1).toList.map((x: Int) => Pixel(x, 0, color)))
    }

    def vLine(length: Int, color: Int = BImage.Black): Pattern = {
        val l0 = length / 2
        val l1 = length - l0 - 1
        new Pattern((-l0 to l1).toList.map((y: Int) => Pixel(0, y, color)))
    }

    def disk(radius: Int, color: Int = BImage.Black): Pattern = {
        val r = math.max(radius - 1, 0)
        // println(s"creating a disk of radius $radius")
        new Pattern(
            range2D(-r to r, -r to r).
            filter{ case (x, y) => math.sqrt(x*x + y*y) <= r }.toList.
            map{ case (x, y) => Pixel(x, y, color) }
        )
    }

    def fromImage(img: Image) = {
        val binary = BImage.binarize(img)
        def okay(x: Int, y: Int) = binary.pixel(x, y) == BImage.Black
        val x0 = img.width / 2
        val y0 = img.height / 2
        new Pattern(
            range2D(0 until img.width, 0 until img.height).
            filter((xy: (Int, Int)) => okay(xy._1, xy._2)).toList.
            map{ case (x, y) => Pixel(x - x0, y - y0, BImage.Black) }
        )
    }

    def notePattern(height: Int) = {
        val img = Image(new File("partitions/note_sample.png"))
        val ratio = height.toDouble / img.height
        val width = (ratio * img.width).toInt
        fromImage(img.scaleTo(width, height))
    }

    val noteRatio = 7.0 / 6.0
}
