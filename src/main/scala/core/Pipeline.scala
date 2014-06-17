package core

import com.sksamuel.scrimage.Image
import java.awt.image.BufferedImage
import java.io.File
import breeze.linalg.{DenseVector, DenseMatrix}
import impl.MatrixConversion.imageToMatrix
import impl.EasyIO.{WriteAndClose, FileAsInt}

class Pipeline(val catNN: NeuralNetwork, val durationNN: NeuralNetwork) {
    val width = Pipeline.width
    val height = Pipeline.height
    private val firstLine = Pipeline.firstLine
    private val lastLine = Pipeline.lastLine
    private var img: Option[Image] = None

    val trainingDirs = List("data")
    val outputDir = "collected"
    val counter = outputDir + '/' + "counter.txt"
    private var index: Int = counter.readAsInt

    def train(){
        val categories = load(outputDir :: trainingDirs, (n: Note) => true, _.cat).toList
        catNN.train(categories, 200, 10, (x: Int) => s"layers/note_$x.txt")
        val durations = load(outputDir :: trainingDirs, _.isNote, _.duration).toList
        durationNN.train(durations, 200, 10, (x: Int) => s"layers/duration_$x.txt")
    }

    def getPrediction(buff: BufferedImage) = {
        val centered: Image = BinaryImage.centerOnLines(Image(buff), width, height, firstLine, lastLine)
        val x = castToVector(centered)
        val y = durationNN.getPredictedClass(x)
        img = Some(centered)
        (y, centered.awt)
    }

    private def load(dirs: Iterable[String],
             noteFilter: Note => Boolean, toInteger: Note => Int) = {

        def desc_filter(img_desc: (String, String)) = noteFilter(Note.fromFile(img_desc._2))
        def desc_toInt(desc: String) = toInteger(Note.fromFile(desc))

        for (img_desc <- DataManipulator.listImagesDescriptions(dirs).filter(desc_filter))
            yield (loadXFromFile(img_desc._1), desc_toInt(img_desc._2))
    }

    private def loadXFromFile(img: String) : DenseVector[Double] = {
        castToVector(BinaryImage.centerOnLines(Image(new File(img)), width, height, firstLine, lastLine))
    }

    private def castToVector[T](matrix: DenseMatrix[T]) = new DenseVector[T](matrix.data)

    def accept(t: Int) = img.foreach(dumpExample(t, _))

    def dumpExample(y: Int, img: Image) = {
        outputDir + s"/img_$index.txt" <<| s"$index;Note;_;_;_;${Note.durationDescription(y)};_"
        img.write(outputDir + s"/img_$index.png")
        index += 1
        counter <<| s"$index"
    }
}

object Pipeline {
  
    val width = 20
    val height = 70
    private val firstLine = 20
    private val lastLine = 50 

    def fromFile(noteLayers: String, durationLayers: String) = 
        new Pipeline(NeuralNetwork.fromFile(noteLayers), NeuralNetwork.fromFile(durationLayers))

    def empty() = { new Pipeline(
            new NeuralNetwork(width*height, 10, Note.catDescription.length),
            new NeuralNetwork(width*height, 15, Note.durationDescription.length)
        ) }   
}