package core

import java.io.FileInputStream

import breeze.linalg.{DenseMatrix, DenseVector}
import com.sksamuel.scrimage.Image
import com.sksamuel.scrimage.ScaleMethod.Bicubic
import impl.MatrixConversion.imageToMatrix

/**
 * Created by Guillaume on 03/06/2014.
 */
object DataLoader {

    def load(dir: String, width: Int, height: Int,
             firstLine: Int, lastLine: Int,
             noteFilter: Note => Boolean, toInteger: Note => Int) = {

        def desc_filter(img_desc: (String, String)) = noteFilter(Note.fromFile(dir+'/'+img_desc._2))
        def desc_toInt(desc: String) = toInteger(Note.fromFile(dir+'/'+desc))
        def getVector(img: String) = castToVector(
            loadImageClipAndPad(dir+'/'+img, width, height, firstLine, lastLine))
        
        for (img_desc <- DataManipulator.listImagesDescriptions(List(dir)).filter(desc_filter)) 
            yield (getVector(img_desc._1), desc_toInt(img_desc._2))
    }

    def loadImageClipAndPad(filename: String, width: Int, height: Int, firstLine: Int, lastLine: Int) : DenseMatrix[Double] = {
        val img = Image(new FileInputStream(filename))
        BinaryImage.centerOnLines(img, width, height, firstLine, lastLine)
    }

    def castToVector[T](matrix: DenseMatrix[T]) = {
        new DenseVector[T](matrix.data)
    }

    def loadImage(filename: String, width: Int, height: Int) = {
        Image(new FileInputStream(filename)).scaleTo(width, height, Bicubic)
    }

    def loadImage(filename: String) = {
        Image(new FileInputStream(filename))
    }

}

