import java.io._
import java.util.Scanner
import scala.language.implicitConversions
import scala.collection.JavaConversions._

/**
 * Created by guillaume on 26/05/14.
 */
object DataManipulator {

    implicit def file(s: String) = new File(s)

    // will be converted to new File(String)

    implicit class WriteAndClose(output: String) {
        def <<|(f: Writer => Unit) = {
            val writer = new FileWriter(output)
            f(writer)
            writer.close()
        }
    }

    def aggregate(directory: String) = { writer: Writer =>
        directory.list().filter(_.endsWith(".png")).foreach {
            filename: String => copyFileIn(directory + '/' + getTxt(filename), writer)
        }
    }

    def copyFileIn(file: File, writer: Writer) {
        io.Source.fromFile(file).getLines().foreach { lines => writer.write(lines + '\n')}
    }

    def copyFile(src: File, dest: File) {
        new FileOutputStream(dest) getChannel() transferFrom(new FileInputStream(src).getChannel, 0, Long.MaxValue)
    }

    def copyImgAndTxt(filename: String, newFilename: String, newIndex: Int) {
        val rewriteTxt = { writer: Writer =>
            val line = new Scanner(new FileReader(getTxt(filename))).nextLine()
            writer.write(s"$newIndex ${line.split(" ", 2)(1)}\n")
        }
        newFilename + s"_$newIndex.txt" <<| rewriteTxt
        copyFile(getImage(filename), newFilename + s"_$newIndex.png")
    }

    def getPrefix(filename: String) = filename.split('.')(0)

    def getImage(filename: String) = getPrefix(filename) + ".png"

    def getTxt(filename: String) = getPrefix(filename) + ".txt"

    def mergeDirectories(directories: Iterable[String], outputFileName: String) = {
        var i_output = 1
        for (dir <- directories) {
            for (file <- dir.list().filter(_.endsWith(".png"))) {
                copyImgAndTxt(file, outputFileName, i_output)
                i_output += 1
            }
        }
        i_output -= 1
        println(s"Found $i_output files, copied them to " + outputFileName)
        i_output
    }


    def main(args : Array[String]){
        "duration/description.txt" <<| aggregate("duration")
        //        val iMax = mergeFolders(Array("jerusalem/img", "croches/img"), "data/croche")
        //        aggregate("data/croche", 1, iMax)
    }

}
