package core

import com.sksamuel.scrimage.{Image, X11Colorlist}
// import java.awt.image.BufferedImage
import java.io.File
import breeze.linalg.{DenseVector, DenseMatrix}
import impl.MatrixConversion.imageToMatrix
import impl.EasyIO.{WriteAndClose, FileCounter}
import core.NoteFinder.NoteBlock

class Pipeline(val somethingNN: NeuralNetwork, val durationNN: NeuralNetwork) {
    val width = Pipeline.width
    val height = Pipeline.height
    private val firstLine = Pipeline.firstLine
    private val lastLine = Pipeline.lastLine
    private var img: Image = null

    val trainingDirs = List("data", "nothing", "collected")
    val outputDir = "collected"
    val counter = new FileCounter(outputDir)

    def train(){
        // val categories = load(outputDir :: trainingDirs, (n: Symbol) => true, _.isSomething).toList
        // somethingNN.train(categories, 100, 10, (x: Int) => s"layers/note_$x.txt")
        // // NeuralNetwork.visualizeAllHiddenLayers(somethingNN, width, height)
        // val durations = load(outputDir :: trainingDirs, _.isNote, _.duration).toList
        // durationNN.train(durations, 200, 10, (x: Int) => s"layers/duration_$x.txt")
        val simpleCat = load(trainingDirs, (n: Symbol) => true, _.simpleCat).toList
        durationNN.train(simpleCat, 200, 10, (x: Int) => s"layers/duration_$x.txt")
        durationNN.visualizeAllHiddenLayers(width, height)
    }

    def getPrediction(input: NoteBlock) : Symbol = {
        // val centered: Image = BinaryImage.centerOnLines(input.img, width, height, firstLine, lastLine)
        val x = castToVector(input.img.scaleTo(width, height))
        // val isSomething = somethingNN.getPredictedClass(x) > 0
        val y = Symbol.fromSimpleCat(durationNN.getPredictedClass(x))
        y match {
            case n: Note => Note(input.level, n.duration, n.annotation, n.pointed)
            case _ => y
        }
    }

    def getCurrentImage(width: Int, height: Int) = img.fit(width, height).awt

    private def load(dirs: Iterable[String],
             noteFilter: Symbol => Boolean, toInteger: Symbol => Int) = {

        def desc_filter(img_desc: (String, String)) = noteFilter(Symbol.fromFile(img_desc._2))
        def desc_toInt(desc: String) = toInteger(Symbol.fromFile(desc))

        for (img_desc <- DataManipulator.listImagesDescriptions(dirs).filter(desc_filter))
            yield (loadXFromFile(img_desc._1), desc_toInt(img_desc._2))
    }

    private def loadXFromFile(img: String) : DenseVector[Double] = {
        castToVector(BinaryImage.centerOnLines(Image(new File(img)), width, height, firstLine, lastLine))
    }

    private def castToVector[T](matrix: DenseMatrix[T]) = new DenseVector[T](matrix.data)

    def accept(n: Symbol) = dumpNote(n, img)

    def dumpNote(n: Symbol, img: Image) = {
        outputDir + s"/img_${counter.index}.txt" <<| n.toString
        img.write(outputDir + s"/img_${counter.index}.png")
        counter += 1
    }

    def loadImageAndSplit(img: Image) = {
        val chunks = NoteFinder.splitByLines(img)
        val noteBlocks = chunks.flatMap(NoteFinder.findBlockAndLevels)
        val imgWithTag = img.toMutable
        for(nB <- noteBlocks){
            imgWithTag.drawRect(nB.x0, nB.y0, nB.img.width, nB.img.height, X11Colorlist.Red)
        }
        (imgWithTag, noteBlocks)
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
            // new NeuralNetwork(width*height, 15, Symbol.durationDescription.length)
            new NeuralNetwork(width*height, 20, Symbol.simpleCatNumber)
        ) }   
}