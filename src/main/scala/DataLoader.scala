import breeze.linalg.{DenseVector, DenseMatrix}
import com.sksamuel.scrimage.Image
import com.sksamuel.scrimage.ScaleMethod.Bicubic
import java.io.{FileInputStream, InputStream}
import ImplicitMatrixConversion.{imageToMatrix, imageToBinary}

/**
 * Created by Guillaume on 03/06/2014.
 */
object DataLoader {

    def loadAsRaw(filename: String, imageName: String, width: Int, height: Int) = {
        val descriptions = io.Source.fromFile(filename).getLines()
        val notes = for (description <- descriptions) yield new Note(description)
        def getVector(n: Note) = castToVector(loadImage(imageName + s"_${n.index}.png", width, height))
        for (note <- notes) yield (getVector(note), note)
    }

    def load(dir: String, width: Int, height: Int,
             firstLine: Int, lastLine: Int,
             noteFilter: Note => Boolean, toInteger: Note => Int) = {


        def desc_filter(img_desc: (String, String)) = noteFilter(Note.fromFile(dir+'/'+img_desc._2))
        def desc_toInt(desc: String) = toInteger(Note.fromFile(dir+'/'+desc))
        def getVector(img: String) = castToVector(
            loadImageClipAndPad(dir+'/'+img, width, height, firstLine, lastLine))
        
        for (img_desc <- DataManipulator.listImagesDescriptions(dir).filter(desc_filter)) 
            yield (getVector(img_desc._1), desc_toInt(img_desc._2))
    }

    def loadImageClipAndPad(filename: String, width: Int, height: Int, firstLine: Int, lastLine: Int) : DenseMatrix[Double] = {
        val img = Image(new FileInputStream(filename))
        val xys = img.findLines().toList
        if(xys.isEmpty) return img scaleTo(width, height, Bicubic)
        var first : Int = xys.head._2
        val last : Int = xys.last._2
        val a : Double = (firstLine - lastLine).toDouble / (first - last)
        var shrinked : DenseMatrix[Double] = img scaleTo(width, (img.height.toDouble*a).toInt, Bicubic)
        // first -> a*first
        first = (a*first).toInt
        val leftCols = firstLine - first
        val rightCols = height - shrinked.cols - leftCols
        if (leftCols > 0){
            shrinked = pad(shrinked, 0, 0, leftCols, 0)
        } else if (leftCols < 0) {
            shrinked = crop(shrinked, 0, 0, -leftCols, 0)
        }
        if (rightCols > 0){
            shrinked = pad(shrinked, 0, 0, 0, rightCols)
        } else if (rightCols < 0) {
            shrinked = crop(shrinked, 0, 0, 0, -rightCols)
        }
        return shrinked
    }

    private def pad(mat: DenseMatrix[Double], topRows: Int, bottomRows: Int, leftCols: Int, rightCols: Int) = {
        val padded = DenseMatrix.zeros[Double](mat.rows + topRows + bottomRows, mat.cols + leftCols + rightCols)
        padded(topRows until topRows + mat.rows, leftCols until leftCols + mat.cols) := mat
        padded
    }

    private def crop[T](mat: DenseMatrix[T], topRows: Int, bottomRows: Int, leftCols: Int, rightCols: Int) = {
        val croped = mat(topRows until mat.rows - bottomRows, leftCols until mat.cols - rightCols)
        croped
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

