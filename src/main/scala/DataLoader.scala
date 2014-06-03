import breeze.linalg.{DenseVector, DenseMatrix}
import com.sksamuel.scrimage.Image
import com.sksamuel.scrimage.ScaleMethod.Bicubic
import java.io.{FileInputStream, InputStream}

/**
 * Created by Guillaume on 03/06/2014.
 */
object DataLoader {

    def load(filename: String, firstIndex: Int, lastIndex: Int,
             width: Int, height: Int,
             noteFilter: Note => Boolean, toInteger: Note => Int) = {
        val descriptions = io.Source.fromFile(filename + s"_${firstIndex}_$lastIndex.txt").getLines
        val notes = (for (description <- descriptions) yield new Note(description)).filter(noteFilter)

        def getVector(n: Note) = imageAsVector(filename + s"_${n.index}.png", width, height)

        for (note <- notes) yield (getVector(note), toInteger(note))
    }

    def imageAsVector(filename: String, width: Int, height: Int) = {
        val in: InputStream = new FileInputStream(filename)
        val image = Image(in).scaleTo(width, height, Bicubic)
        new DenseVector[Int](image.pixels).map(rgbToGrayScale)
    }

    def rgbToGrayScale(r: Int, g: Int, b: Int): Double = (0.2126 * r + 0.7152 * g + 0.0722 * b) / 255

    def rgbToGrayScale(rgb: Int): Double = rgbToGrayScale(rgb & 0xFF0000, rgb & 0x00FF00, rgb & 0x0000FF)
}

