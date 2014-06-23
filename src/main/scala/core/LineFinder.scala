package core

import com.sksamuel.scrimage.{Image, MutableImage, X11Colorlist, Color}
import java.awt.Graphics2D
import java.io.File

object LineFinder {

    val Black = 0xFF000000
    val White = 0xFFFFFFFF

    def binarize(img: Image) = {
        def rgbToGray(r: Int, g: Int, b: Int): Int = (0.2126 * r + 0.7152 * g + 0.0722 * b).toInt
        def rgbToGrayScale(rgb: Int): Int = rgbToGray(rgb & 0xFF0000 >> 16, rgb & 0x00FF00 >> 8, rgb & 0x0000FF)
        def binarize(rgb: Int) = if(rgbToGrayScale(rgb) < 150) Black else White

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
        lines.filter((line: Line) => img.pixel(line.x, line.y) == Black && img.width*0.8 < line.length)
        // lines
    }

    def extractBlackLines(img: Image) = {
        val binarized = binarize(img)
        val lines = findBlackLines(binarized)
        val extracted = Image.filled(img.width, img.height, X11Colorlist.White)
        def fill(line: Line) = 
            extracted._fill(line.x, line.y, line.length, 1, X11Colorlist.Red)
        lines.foreach(fill)
        lines.foreach(println)
        extracted
    }

    case class Chunk(img: Image, line0: Line, line1: Line)
    def splitByLines(img: Image) = {
        val lines = findBlackLines(binarize(img)).toArray
        val iMax = lines.length
        var i = 0

        def findLine() = {
            if(i >= iMax) throw new java.lang.IndexOutOfBoundsException()
            val l0 = lines(i)
            var yi = l0.y
            var n = 1
            var yMean = l0.y
            i += 1
            while(i < iMax && lines(i).y == yi + 1){
                n += 1
                yi = lines(i).y
                yMean += yi
                i += 1
            }
            Line(l0.x, yMean / n, l0.length)
        }

        def findBlock() : Option[Chunk] = {
            try{
                val l0 = findLine()
                val i0 = i
                val l1 = findLine()
                val deltaMin = 0.7 * (l1.y - l0.y)
                val deltaMax = 1.3 * (l1.y - l0.y)
                val l2 = findLine()
                val l3 = findLine()
                val l4 = findLine()
                
                if(    (l2.y - l1.y < deltaMin) 
                    || (l2.y - l1.y > deltaMax)
                    || (l3.y - l2.y < deltaMin)
                    || (l3.y - l2.y > deltaMax)
                    || (l4.y - l3.y < deltaMin)
                    || (l4.y - l3.y > deltaMax) 
                ){
                    println("line skipped")
                    i = i0
                    findBlock()
                } else {
                    val top = l0.y - (3 * (l1.y - l0.y) )
                    val height = 10 * (l1.y - l0.y)
                    println(s"Found block with ${l0.y}, ${l1.y}, ${l2.y}, ${l3.y}, ${l4.y}")
                    Some(Chunk(img.subimage(l0.x, top, l0.length, height), l0, l1))
                }
            } catch {
                case (e: java.lang.IndexOutOfBoundsException) => println("No more lines"); None
            }       
        }

        var chunks : List[Chunk] = Nil
        var hasNextLine = true
        while(hasNextLine){
            findBlock() match {
                case (Some(chunk: Chunk)) => chunks = chunk :: chunks
                case None => hasNextLine = false
            }
        }

        chunks.reverse
    }

    // def 

    def main(args: Array[String]) {
        def rename(name: String, suffix: String) = {
            val splitted = name.split("\\.", 2)
            splitted(0) + '_' + suffix + '.' + splitted(1)
        }
        val file = "partitions/andantino_1.png"
        val in = new File(file)
        // val out = new File(rename(file, "lines"))
        // extractBlackLines(Image(in)).write(out)
        // val out2 = new File(rename(file, "binarized"))
        // binarize(Image(in)).write(out2)
        
        def saveChunck(chunk_index: (Chunk, Int) ) : Unit = {
            val i = chunk_index._2
            val out = new File(rename(file, s"chunck_$i"))
            val img = chunk_index._1.img
            val scaled = img  /*.scaleTo(2*img.width, 2*img.height)*/
            scaled.write(out)
            val diameter = (/*2 **/ (chunk_index._1.line1.y - chunk_index._1.line0.y)).toInt
            // val radius = (0.3 * diameter).toInt
            val epured = Pattern.vLine(2*diameter).erase(binarize(scaled))
            val smallLine = Pattern.hLine((1.5 * Pattern.noteRatio * diameter).toInt)
            val bigNote = Pattern.notePattern(diameter)
            val out2 = new File(rename(file, s"chunck_${i}_closed"))
            bigNote.erode(smallLine.close(epured)).write(out2)
        }

        splitByLines(Image(in)).zipWithIndex.map(saveChunck)

    }




}

import impl.EasyMath.{EasyIterable, range2D}

case class Pixel(x: Int, y: Int, color: Int)
class Pattern(val pixels: List[Pixel], val color: Int){

    val width = pixels.maxValue[Int]((p: Pixel) => math.abs(p.x))
    val height = pixels.maxValue[Int]((p: Pixel) => math.abs(p.y))

    def this(pixels: List[Pixel]) = this(pixels, pixels(0).color)

    def fitAt(img: Image, x0: Int, y0: Int) = {
        pixels.forAll((p: Pixel) => img.pixel(x0+p.x, y0+p.y) == p.color)
    }

    def anchorAt(img: Image, x0: Int, y0: Int) = img.pixel(x0, y0) == color

    def drawAt(img: MutableImage, x0: Int, y0: Int) = {
        for(p <- pixels) img.setPixel(x0 + p.x, y0 + p.y, p.color)
    }
    
    def clearAt(img: MutableImage, x0: Int, y0: Int,  background: Int = LineFinder.White) = {
        for(p <- pixels) img.setPixel(x0 + p.x, y0 + p.y, background)
    }

    def dilate(img: Image) = {
        val dilated = Image(img).toMutable
        for(x <- width until (img.width - width);
            y <- height until (img.height- height)){
            if(anchorAt(img, x, y)) drawAt(dilated, x, y)
        }
        dilated
    }

    def erode(img: Image, background: Color = X11Colorlist.White) = {
        val eroded = Image.filled(img.width, img.height, background).toMutable
        for(x <- width until (img.width - width);
            y <- height until (img.height- height)){
            if(fitAt(img, x, y)) drawAt(eroded, x, y)
        }
        eroded
    }

    def erase(img: Image, background: Int = LineFinder.White) = {
        val erased = MutableImage(img)
        for(x <- width until (img.width - width);
            y <- height until (img.height- height)){
            if(fitAt(img, x, y)) clearAt(erased, x, y)
        }
        erased
    }

    def close(img: Image) = erode(dilate(img))
    def open(img: Image) = dilate(erode(img))
}

object Pattern {
    def hLine(length: Int, color: Int = LineFinder.Black): Pattern = {
        val l0 = length / 2
        val l1 = length - l0 - 1
        new Pattern((-l0 to l1).toList.map((x: Int) => Pixel(x, 0, color)))
    }

    def vLine(length: Int, color: Int = LineFinder.Black): Pattern = {
        val l0 = length / 2
        val l1 = length - l0 - 1
        new Pattern((-l0 to l1).toList.map((y: Int) => Pixel(0, y, color)))
    }

    def disk(radius: Int, color: Int = LineFinder.Black): Pattern = {
        val r = math.max(radius - 1, 0)
        // println(s"creating a disk of radius $radius")
        new Pattern(
            range2D(-r to r, -r to r).
            filter{ case (x, y) => math.sqrt(x*x + y*y) <= r }.toList.
            map{ case (x, y) => Pixel(x, y, color) }
        )
    }

    def fromImage(img: Image) = {
        val binary = LineFinder.binarize(img)
        def okay(x: Int, y: Int) = binary.pixel(x, y) == LineFinder.Black
        val x0 = img.width / 2
        val y0 = img.height / 2
        new Pattern(
            range2D(0 until img.width, 0 until img.height).
            filter((xy: (Int, Int)) => okay(xy._1, xy._2)).toList.
            map{ case (x, y) => Pixel(x - x0, y - y0, LineFinder.Black) }
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