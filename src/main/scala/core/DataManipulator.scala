package core

import java.io._
import java.util.Scanner
import scala.language.implicitConversions

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

    def aggregate(dir: String) = { writer: Writer =>
        listImages(dir).foreach {
            filename: String => copyFileIn(dir + '/' + getTxt(filename), writer)
        }
    }

    def copyFileIn(file: File, writer: Writer) {
        io.Source.fromFile(file).getLines().foreach { lines => writer.write(lines + '\n')}
    }

    def copyFile(src: File, dest: File) {
        new FileOutputStream(dest) getChannel() transferFrom(new FileInputStream(src).getChannel, 0, Long.MaxValue)
    }

    def copyImgAndTxt(filename: String, newFilename: String, newIndex: Int) {
        val line = new Scanner(new FileReader(getTxt(filename))).nextLine()
        val rewriteTxt = { writer: Writer =>
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
            for (file <- listImages(dir)) {
                copyImgAndTxt(dir+'/'+file, outputFileName, i_output)
                i_output += 1
            }
        }
        i_output -= 1
        println(s"Found $i_output files, copied them to " + outputFileName)
        i_output
    }

    def listImages(dir: String) = dir.list().filter(_.endsWith(".png")).toIterator.map(dir + '/' + _)
    def listImages(directories: Iterable[String]) : Iterator[String] = {
        directories.map(listImages).fold(Iterator[String]())(_ ++ _)
    }

    def listImagesDescriptions(directories: Iterable[String]) = {
        listImages(directories).map((img: String) => (img, getTxt(img)))
    }

    def main(args : Array[String]){
        "duration/description.txt" <<| aggregate("duration")
        val iMax = mergeDirectories(Array("jerusalem", "duration"), "data/img")
        // aggregate("data/croche", 1, iMax)
    }

}
