package core

import java.io.File

import breeze.linalg.{DenseMatrix, DenseVector}
import com.sksamuel.scrimage.Image
import com.sksamuel.scrimage.ScaleMethod.Bicubic
import impl.MatrixConversion.imageToMatrix
import scala.language.implicitConversions


/**
 * Created by Guillaume on 03/06/2014.
 */
object DataLoader {

    private implicit def file(s: String) = new File(s)

    def load(dir: String, width: Int, height: Int,
             firstLine: Int, lastLine: Int,
             noteFilter: Symbol => Boolean, toInteger: Symbol => Int) = {

        def desc_filter(img_desc: (String, String)) = noteFilter(Symbol.fromFile(dir+'/'+img_desc._2))
        def desc_toInt(desc: String) = toInteger(Symbol.fromFile(dir+'/'+desc))
        def getVector(img: String) = castToVector(
            loadImageClipAndPad(dir+'/'+img, width, height, firstLine, lastLine))
        
        for (img_desc <- listImagesDescriptions(List(dir)).filter(desc_filter)) 
            yield (getVector(img_desc._1), desc_toInt(img_desc._2))
    }

    def loadImageClipAndPad(filename: String, width: Int, height: Int, firstLine: Int, lastLine: Int) : DenseMatrix[Double] = {
        val img = Image(new File(filename))
        BinaryImage.centerOnLines(img, width, height, firstLine, lastLine)
    }

    def castToVector[T](matrix: DenseMatrix[T]) = {
        new DenseVector[T](matrix.data)
    }

    def loadImage(filename: String, width: Int, height: Int) = {
        Image(new File(filename)).scaleTo(width, height, Bicubic)
    }

    def loadImage(filename: String) = {
        Image(new File(filename))
    }

    def getPrefix(filename: String) = filename.split('.')(0)
    def getImage(filename: String) = getPrefix(filename) + ".png"
    def getTxt(filename: String) = getPrefix(filename) + ".txt"

    def listImages(dir: String) = dir.list().filter(_.endsWith(".png")).toIterator.map(dir + '/' + _)
    def listImages(directories: Iterable[String]) : Iterator[String] = {
        directories.map(listImages).fold(Iterator[String]())(_ ++ _)
    }

    def listImagesDescriptions(directories: Iterable[String]) = {
        listImages(directories).map((img: String) => (img, getTxt(img)))
    }

}

