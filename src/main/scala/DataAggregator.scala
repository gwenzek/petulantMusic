import java.io._
import java.util.Scanner
import scala.language.implicitConversions

/**
 * Created by guillaume on 26/05/14.
 */
object DataAggregator {

    def aggregate(filename : String, firstIndex : Int, lastIndex : Int){
        val writer = new FileWriter(filename+s"_${firstIndex}_$lastIndex.txt")
        for(i <- firstIndex to lastIndex){
            try{
                val reader = new Scanner(new FileReader(filename+s"_$i.txt"))
                while(reader.hasNext) {
                    writer.write(reader.nextLine()+'\n')
                }
            } catch {
                case e : IOError => println(s"Problems with file $i.")
                case e : FileNotFoundException => println(s"File $i not found.")
            }
        }
        writer.close()
    }

    def copyFile(src: File, dest: File) {
        new FileOutputStream(dest) getChannel() transferFrom(new FileInputStream(src) getChannel, 0, Long.MaxValue)
    }

    def copyFile(filename: String, newFilename: String, newIndex: Int) {
        val writer = new FileWriter(newFilename + s"_$newIndex.txt")
        val line = new Scanner(new FileReader(filename + ".txt")).nextLine()
        writer.write(s"$newIndex ${line.split(" ", 2)(1)}\n")
        writer.close()
        implicit def file(s: String) = new File(s) // will be converted to new File(String)
        copyFile(filename + ".png", newFilename + s"_$newIndex.png")
    }

    def mergeFolders(filesName: Iterable[String], outputFileName: String) = {
        var i_output = 1
        for (filename <- filesName) {
            var i_input = 1
            try {
                while (true) {
                    copyFile(filename + s"_$i_input", outputFileName, i_output)
                    i_input += 1
                    i_output += 1
                }
            } catch {
                case e@(_: IOError | _: FileNotFoundException) => println(s"Found ${i_input - 1} files in " + filename)
            }
        }
        i_output -= 1
        println(s"Found $i_output files, copied them to " + outputFileName)
        i_output
    }

    def main(args : Array[String]){
        //        aggregate("jerusalem/img", 1, 65)
        val iMax = mergeFolders(Array("jerusalem/img", "croches/img"), "data/croche")
        aggregate("data/croche", 1, iMax)
    }

}
