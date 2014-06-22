package core

import com.sksamuel.scrimage.Image
// import java.awt.image.BufferedImage
import java.io.File
import breeze.linalg.{DenseVector, DenseMatrix}
import impl.MatrixConversion.imageToMatrix
import impl.EasyIO.{WriteAndClose, FileCounter}

class Pipeline(val somethingNN: NeuralNetwork, val durationNN: NeuralNetwork) {
    val width = Pipeline.width
    val height = Pipeline.height
    private val firstLine = Pipeline.firstLine
    private val lastLine = Pipeline.lastLine
    private var img: Image = null

    val trainingDirs = List("data", "nothing")
    val outputDir = "collected"
    val counter = new FileCounter(outputDir)

    def train(){
        val categories = load(outputDir :: trainingDirs, (n: Note) => true, _.isSomething).toList
        somethingNN.train(categories, 100, 10, (x: Int) => s"layers/note_$x.txt")
        // NeuralNetwork.visualizeAllHiddenLayers(somethingNN, width, height)
        val durations = load(outputDir :: trainingDirs, _.isNote, _.duration).toList
        durationNN.train(durations, 200, 10, (x: Int) => s"layers/duration_$x.txt")
        // NeuralNetwork.visualizeAllHiddenLayers(durationNN, width, height)
    }

    def getPrediction(input: Image) = {
        val centered: Image = BinaryImage.centerOnLines(input, width, height, firstLine, lastLine)
        val x = castToVector(centered)
        img = centered
        val isSomething = somethingNN.getPredictedClass(x) > 0
        if(isSomething) Some(durationNN.getPredictedClass(x))
        else None
    }

    def getCurrentImage(width: Int, height: Int) = img.fit(width, height).awt

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

    def accept(n: Note) = dumpNote(n, img)

    def dumpNote(n: Note, img: Image) = {
        outputDir + s"/img_${counter.index}.txt" <<| n.toString
        img.write(outputDir + s"/img_${counter.index}.png")
        counter += 1
    }
}

object Pipeline {
  
    val width = 20
    val height = 80
    private val firstLine = 30
    private val lastLine = 50

    def fromFile(noteLayers: String, durationLayers: String) = 
        new Pipeline(NeuralNetwork.fromFile(noteLayers), NeuralNetwork.fromFile(durationLayers))

    def empty() = { new Pipeline(
            new NeuralNetwork(width*height, 10, 2),
            new NeuralNetwork(width*height, 15, Note.durationDescription.length)
        ) }   
}