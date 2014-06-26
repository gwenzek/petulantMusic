package core

import com.sksamuel.scrimage.{Image, MutableImage, X11Colorlist, Color}
import java.awt.Graphics2D
import java.io.File
import impl.EasyMath._
import BImage._
import Pattern._

object NoteFinder {

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
        // lines.foreach(println)
        extracted
    }

    case class Chunk(img: Image, delta: Int, x0: Int, y0: Int)
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
                    // println("line skipped")
                    i = i0
                    findBlock()
                } else {
                    val delta = (l4.y - l0.y) / 4
                    val top = l0.y - (3 * delta)
                    val height = l4.y + (3 * delta) - top
                    // println(s"Found block with ${l0.y}, ${l1.y}, ${l2.y}, ${l3.y}, ${l4.y}")
                    Some(Chunk(img.subimage(l0.x, top, l0.length, height), delta, l0.x, top))
                }
            } catch {
                case (e: java.lang.IndexOutOfBoundsException) => println("No more lines"); None
            }       
        }

        iterUntilNone(findBlock).toList
    }

    private def epure(chunk: Chunk) : Image = {
        val diameter = chunk.delta
        val epured = binarize(chunk.img).erase(vLine(2*diameter))
        val smallLine = Pattern.hLine((1.3 * Pattern.noteRatio * diameter).toInt)
        val bigNote = Pattern.notePattern(diameter)
        epured.close(smallLine).erode(bigNote)
    }


    private def extractBlock(epured: Image, 
            begin:Int = 0, end_default: Int = -1,
            threshold: Int = 0) : List[(Int, Int)] = {
        val end = if(end_default < 0) epured.width else end_default
        val sumByColumns = Array.ofDim[Int](end - begin)
        for(x <- begin until end; y <- 0 until epured.height){
            if(epured.pixel(x, y) == Black)
                sumByColumns(x - begin) += 1
        }
        // sumByColumns.foreach((x: Int) => print(s"$x, "))
        // println()
        def findBlock(x0: Int, x: Int) : (Int, Int) = {
            if(x >= end) (x0, x)
            else if(sumByColumns(x - begin) <= threshold) (x0, x)
            else findBlock(x0, x+1)
        }
        def skipWhite(x: Int) : Int = {
            if(x >= end) x
            else if(sumByColumns(x - begin) > threshold) x
            else skipWhite(x+1)
        }
        var x = begin
        var blocks: List[(Int, Int)] = Nil
        while(x < end){
            x = skipWhite(x)
            val block = findBlock(x, x)
            x = block._2
            blocks = block :: blocks
        }
        val minWidth = 0.25 * epured.height
        val maxWidth = 0.35 * epured.height

        def checkBlock(x0x1 : (Int, Int)) : List[(Int, Int)] = x0x1 match {
            case (x0: Int, x1: Int) => 
                if(x1 == x0) Nil
                else if(x1 - x0 < minWidth) {
                    val d = ((minWidth - (x1 - x0))/2).toInt
                    // println(s"$x0x1 extended to ${(x0 - d, x0 + d)}")
                    List((x0 - d, x1 + d))
                } else if(x1 - x0 > maxWidth) {
                    val h = sumByColumns.toIterable.minValue[Int]((x:Int) => x) + 1
                    val newNotes = extractBlock(epured, x0, x1, h)
                    // println(s"$x0x1 splitted in $newNotes with threshold $h")
                    newNotes
                } else {
                    List((x0, x1))
                }
        }
        blocks.flatMap(checkBlock).reverse
    }

    case class NoteBlock(img: Image, level: Int, x0: Int, y0: Int)
    def findBlockAndLevels(chunk: Chunk) : List[NoteBlock] = {
        val epured = epure(chunk)
        val blocks = extractBlock(epured)
        def generateNoteBlock(x0x1: (Int, Int)) = x0x1 match {
            case (x0: Int, x1: Int) => 
                val level = findLevel(epured, chunk.delta)(x0, x1)
                val sub = chunk.img.subimage(x0, 0, x1 - x0, chunk.img.height)
                NoteBlock(sub, level, x0 + chunk.x0, chunk.y0)
        }
        blocks.map(generateNoteBlock)
    } 

    private def findLevel(epured: Image, delta: Int)(x0: Int, x1: Int) = {
        val sumByRows = Array.ofDim[Int](epured.height)
        for(x <- x0 until x1; y <- 0 until epured.height){
            if(epured.pixel(x, y) == Black)
                sumByRows(y) += 1
        }
        val maxY = (0 until epured.height).maxArg[Int](y => sumByRows(y))
        val level = (epured.height - maxY) * 20 / epured.height
        level
    }

    def main(args: Array[String]) {
        def rename(name: String, suffix: String) = {
            val splitted = name.split("\\.", 2)
            splitted(0) + '_' + suffix + '.' + splitted(1)
        }
        val file = "partitions/andantino_1.png"
        val in = new File(file)
        val out = new File(rename(file, "combo"))
        // extractBlackLines(Image(in)).write(out)
        // val out2 = new File(rename(file, "binarized"))
        // binarize(Image(in)).write(out2)
        
        val img = Image(in).toMutable
        val chunks = splitByLines(img)
        val noteBlocks = findBlockAndLevels(chunks(0))

        for(nB <- noteBlocks){
            img.drawRect(nB.x0, nB.y0, nB.img.width, nB.img.height, X11Colorlist.Red)
        }

        img.write(out)

        // def saveChunk(chunk_index: (Chunk, Int) ) : Unit = {
        //     val i = chunk_index._2
        //     val out = new File(rename(file, s"chunck_$i"))
        //     val img = chunk_index._1.img.toMutable

        //     val out2 = new File(rename(file, s"chunck_${i}_closed"))
        //     val blocks = extractBlock(epure(chunk_index._1))

        //     for((x0, x1) <- blocks){
        //         img.drawLine(x0, 0, x0, img.height, X11Colorlist.Red)
        //         img.drawLine(x0, 0, x1, img.height, X11Colorlist.Red)
        //         img.drawLine(x1, 0, x1, img.height, X11Colorlist.Red)
        //     }
        //     img.write(out2)
        // }

        // val chunks = splitByLines(Image(in))
        // chunks.zipWithIndex.map(saveChunk)
        // // chunks.map(extractNotes)
    }




}

