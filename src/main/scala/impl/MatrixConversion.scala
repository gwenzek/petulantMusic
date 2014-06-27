package impl

import breeze.linalg.{DenseVector, DenseMatrix}
import com.sksamuel.scrimage.Image
import scala.language.implicitConversions
import core.BinaryImage

object MatrixConversion{

    implicit def matrixToBinary(matrix: DenseMatrix[Double]): BinaryImage = 
        new BinaryImage(matrix.map((x: Double) => if(x > 0.5) 1 else 0))
    
    implicit def binaryToMatrix(binary: BinaryImage): DenseMatrix[Int] = binary.data

    implicit def imageToNMatrix(img: Image): DenseMatrix[Double] = {
        new DenseMatrix[Int](img.width, img.height, img.pixels).map(rgbToNGrayScale)
    }

    implicit def imageToBinary(img: Image): BinaryImage = matrixToBinary(imageToNMatrix(img))

    def rgbToNGrayScale(r: Int, g: Int, b: Int): Double = 1.0 - (0.2126 * r + 0.7152 * g + 0.0722 * b) / 256

    def rgbToNGrayScale(rgb: Int): Double = rgbToNGrayScale(rgb & 0xFF0000 >> 16, rgb & 0x00FF00 >> 8, rgb & 0x0000FF)
}