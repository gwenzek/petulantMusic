import com.sksamuel.scrimage.Image
import breeze.linalg.{DenseVector, DenseMatrix}
import java.io.{FileInputStream, InputStream}
import breeze.plot._
import ImplicitMatrixConversion.{imageToMatrix, matrixToBinary, imageToBinary}


class BinaryImage(val data: DenseMatrix[Int]) /*extends DenseMatrix[Int]*/{

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

    def rgbToGrayScale(r: Int, g: Int, b: Int): Double = 1.0 - (0.2126 * r + 0.7152 * g + 0.0722 * b) / 255

    def rgbToGrayScale(rgb: Int): Double = rgbToGrayScale(rgb & 0xFF0000 >> 16, rgb & 0x00FF00 >> 8, rgb & 0x0000FF)

    implicit def binaryToMatrix(binary: BinaryImage): DenseMatrix[Int] = binary.data
    // implicit def matrixToBinary(matrix: DenseMatrix[Int]): BinaryImage = new BinaryImage(matrix)

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
        val cliped = DataLoader.loadImageClipAndPad(filename, 20, 70, 20, 50)
        figure.subplot(0) += image(original)
        figure.subplot(1) += image(cliped)
    }
}