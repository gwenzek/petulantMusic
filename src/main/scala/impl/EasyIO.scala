package impl

import java.io._
import scala.io.Source
import scala.language.implicitConversions


object EasyIO{
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

    case class FileCounter(dir: String){
        val counter = dir+'/'+".counter"
        def index() : Int = 
            try{ Source.fromFile(counter).getLines.next.toInt }
            catch{ case e: java.io.FileNotFoundException => 0 }
        def +=(i: Int) = counter <<| (index + i).toString
        def getFile(prefix: String) = dir+'/'+prefix+'_'+index
    }
}