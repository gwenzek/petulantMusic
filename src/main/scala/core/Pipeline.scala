package core

import com.sksamuel.scrimage.Image
import java.awt.image.BufferedImage
import impl.MatrixConversion.imageToMatrix
import java.io._
import java.util.Scanner
import scala.language.implicitConversions
import breeze.linalg.{DenseVector, DenseMatrix}

class Pipeline(val nn: NeuralNetwork) {
    private val width = 20
    private val height = 70
    private val firstLine = 20
    private val lastLine = 50    
    private var img: Option[Image] = None

    val trainingDirs = List("data")
    val outputDir = "collected"
    val counter = outputDir + '/' + "counter.txt"
    private var index: Int = readIndex

    def this(file: String) = this(NeuralNetwork.fromFile(file))

    def train(){
        val xtl = load(outputDir :: trainingDirs, _.isNote, _.duration).toList
        nn.train(xtl, 200, 10)
    }

    def getPrediction(buff: BufferedImage) = {
        val centered: Image = BinaryImage.centerOnLines(Image(buff), width, height, firstLine, lastLine)
        val x = castToVector(centered)
        val y = nn.getPredictedClass(x)
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

    def readIndex = {
        val scanner = new Scanner(new FileReader(counter))
        scanner.nextInt
    }

    implicit class WriteAndClose(output: String) {
        def <<| (s: String) = {
            val writer = new FileWriter(output)
            writer.write(s)
            writer.close()
        }

        def <<| (messages: Iterable[String]) {
            val writer = new FileWriter(output)
            messages.foreach((s: String) => writer.write(s + '\n'))
            writer.close()
        }
    }
}