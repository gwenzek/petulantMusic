import breeze.linalg.{DenseVector, DenseMatrix}
import com.sksamuel.scrimage.Image
import scala.language.implicitConversions


object ImplicitMatrixConversion{

    implicit def matrixToBinary(matrix: DenseMatrix[Double]): BinaryImage = 
        new BinaryImage(matrix.map((x: Double) => if(x > 0.5) 1 else 0))
    
    implicit def imageToMatrix(img: Image): DenseMatrix[Double] = {
        new DenseMatrix[Int](img.width, img.height, img.pixels).map(rgbToGrayScale)
    }

    implicit def imageToBinary(img: Image): BinaryImage = matrixToBinary(imageToMatrix(img))

    def rgbToGrayScale(r: Int, g: Int, b: Int): Double = 1.0 - (0.2126 * r + 0.7152 * g + 0.0722 * b) / 256

    def rgbToGrayScale(rgb: Int): Double = rgbToGrayScale(rgb & 0xFF0000 >> 16, rgb & 0x00FF00 >> 8, rgb & 0x0000FF)
}