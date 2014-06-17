package core

import breeze.linalg.DenseMatrix
import breeze.plot._
import com.sksamuel.scrimage.Image
import com.sksamuel.scrimage.ScaleMethod.Bicubic
import com.sksamuel.scrimage.Color //yeah !
import impl.MatrixConversion.{binaryToMatrix, imageToBinary, imageToMatrix}
import scala.language.implicitConversions

class BinaryImage(val data: DenseMatrix[Int]){

    private def pixelMatchPattern(x: Int, y: Int, pattern: BinaryImage) : Boolean = {     
        for(i <- 0 until pattern.rows){
            for(j <- 0 until pattern.cols){
                if(this(x+i, y+j) != pattern(i, j)) return false
            }
        }
        return true
    }

    def findPatterns(pattern: BinaryImage) = {
        for(x <- 0 to this.rows - pattern.rows; 
            y <- 0 to this.cols - pattern.cols
            if pixelMatchPattern(x, y, pattern) )
        yield (x, y)
    }

    def findLines() = findPatterns(BinaryImage.linePattern(this.rows))

    def extractPatterns(pattern: BinaryImage) : DenseMatrix[Int] = {
        val mat = DenseMatrix.zeros[Int](this.rows, this.cols)
        for((x, y) <- findPatterns(pattern)){
            mat(x until (x+pattern.rows), y until (y+pattern.cols)) := pattern.data
        }
        return mat
    }

    def removeLines() = this.data -= extractPatterns(BinaryImage.linePattern(this.rows))

    def visualize(slot: Int = 0)(implicit f: Figure) {
        f.subplot(slot) += image(data.map(_.toDouble))
    }

    def printMatrix(){
        for(i <- 0 until this.rows){
            for(j <- 0 until this.cols){
                if(this(i, j)==0) print('.')
                else if(this(i, j)==1) print('#')
                else print('?')
            }
            println()
        }
    }
}

object BinaryImage{

    

    def linePattern(rows: Int) = new BinaryImage(DenseMatrix.ones[Int](rows, 1))

    implicit val figure = new Figure("notes", 1, 2)

    def main(args: Array[String]){
        // test_removeLines()
        test_loadAndPad()
    }

    def test_removeLines(){
        val filename = "jerusalem/img_3.png"
        val img : BinaryImage = DataLoader.loadImage(filename)
        println(s"size = (${img.rows},${img.cols})")
        img.visualize(0)
        img.printMatrix()
        println
        img.removeLines()
        img.visualize(1)
        img.printMatrix()
    }

    def test_loadAndPad(){
        val filename = "jerusalem/img_3.png"
        val original = DataLoader.loadImage(filename)
        val clipped = DataLoader.loadImageClipAndPad(filename, 20, 70, 20, 50)
        figure.subplot(0) += image(original)
        figure.subplot(1) += image(clipped)
    }

    def padTo(img: Image, left: Int, top: Int, right: Int, bottom: Int): Image = {
        val w = img.width + left + right
        val h = img.height + top + bottom
        val filled = Image.filled(w, h, 0xFFFFFF)
        val g = filled.awt.getGraphics
        g.drawImage(img.awt, left, top, null)
        g.dispose()
        filled
    }

    def centerOnLines(img: Image, width: Int, height: Int, firstLine: Int, lastLine: Int) : Image = {
        val xys = img.findLines().toList
        if(xys.length < 2) return img scaleTo(width, height, Bicubic)
        var first : Int = xys.head._2
        val last : Int = xys.last._2
        val a : Double = (firstLine - lastLine).toDouble / (first - last)
        var shrinked : Image = img scaleTo(width, (img.height.toDouble*a).toInt, Bicubic)
        val f = new Figure("shrinked", 2, 2)
        // first -> a*first
        first = (a*first).toInt
        val top = firstLine - first
        val bottom = height - shrinked.height - top
        if (top > 0){
            shrinked = shrinked.padWith(0, top, 0, 0)
        } else if (top < 0) {
            shrinked = shrinked.trim(0, -top, 0, 0)
        }
        if (bottom > 0){
            shrinked = shrinked.padWith(0, 0, 0, bottom)
        } else if (bottom < 0) {
            shrinked = shrinked.trim(0, 0, 0, -bottom)
        }
        return shrinked
    }

}