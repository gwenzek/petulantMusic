package impl

import java.io._
import scala.language.implicitConversions
import java.util.Scanner

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

    implicit class FileAsInt(input: String) {
        def readAsInt () = {
            val scanner = new Scanner(new FileReader(input))
            scanner.nextInt
        }
    }
}