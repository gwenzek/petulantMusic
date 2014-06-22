package core

import com.sksamuel.scrimage.{Image, X11Colorlist, Color}
import java.awt.Graphics2D
import java.io.File

object LineFinder {

    def binarize(img: Image) = {
        def rgbToGray(r: Int, g: Int, b: Int): Int = (0.2126 * r + 0.7152 * g + 0.0722 * b).toInt
        def rgbToGrayScale(rgb: Int): Int = rgbToGray(rgb & 0xFF0000 >> 16, rgb & 0x00FF00 >> 8, rgb & 0x0000FF)
        def binarize(rgb: Int) = if(rgbToGrayScale(rgb) < 200) 0xFF000000 else 0xFFFFFFFF

        img.map( (x: Int, y: Int, rgb: Int) => binarize(rgb) )
    }

    case class Line(x: Int, y: Int, length: Int)
    def findMaxSegment(img: Image, y: Int) = {
        def findEndOfSegment(x: Int): Int =
            if(x >= img.width - 1 || img.pixel(x, y) != img.pixel(x+1, y)) x 
            else findEndOfSegment(x+1)

        var x = 0
        var bestX0 = 0
        var bestL = -1
        while(x < img.width){
            val x0 = x
            x = findEndOfSegment(x)
            if(bestL < x - x0 + 1) {
                bestL = x - x0 + 1; bestX0 = x0
            }
            x += 1
        }
        Line(bestX0, y, bestL)
    }

    def findBlackLines(img: Image) = {
        val lines = for(y <- 0 until img.height) yield findMaxSegment(img, y)
        lines.filter((line: Line) => img.pixel(line.x, line.y) == 0xFF000000 && img.width*0.8 < line.length)
        // lines
    }

    def extractBlackLines(img: Image) = {
        val binarized = binarize(img)
        val lines = findBlackLines(binarized)
        val extracted = Image.filled(img.width, img.height, X11Colorlist.White)
        def fill(line: Line) = 
            _fill(extracted, line.x, line.y, line.length, 1, X11Colorlist.Red)
        lines.foreach(fill)
        lines.foreach(println)
        extracted
    }

    def _fill(img: Image, x: Int, y: Int, w: Int, h: Int, color: Color): Image = {
        val g2 = img.awt.getGraphics.asInstanceOf[Graphics2D]
        g2.setColor(color)
        g2.fillRect(x, y, w, h)
        g2.dispose()
        img
    }

    def main(args: Array[String]) {
        def rename(name: String, suffix: String) = {
            val splitted = name.split("\\.", 2)
            splitted(0) + '_' + suffix + '.' + splitted(1)
        }
        val file = "partitions/aroundTheSun.png"
        val in = new File(file)
        val out = new File(rename(file, "lines"))
        extractBlackLines(Image(in)).write(out)
        val out2 = new File(rename(file, "binarized"))
        binarize(Image(in)).write(out2)

        val out3 = new File(rename(file, "random"))
        val img = Image.filled(200, 100, X11Colorlist.White)
        _fill(img, 0, 10, 50, 1, X11Colorlist.Red)
        img.write(out3)
    }


}