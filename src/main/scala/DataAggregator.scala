import java.io.{FileNotFoundException, IOError, FileReader, FileWriter}
import java.util.Scanner

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
        writer.close
    }

    def main(args : Array[String]){
        aggregate("jerusalem/img", 1, 65)
    }

}
