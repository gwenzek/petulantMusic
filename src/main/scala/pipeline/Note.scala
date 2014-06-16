package pipeline
/**
 * Created by Guillaume on 03/06/2014.
 */

import scala.io.Source


class Note(description: String) {
    private val desc = description.split(";")
    val index = desc(0).toInt
    val properties: Map[String, String] = Map((for (i <- 1 until desc.length) yield (Note.propertiesDef(i - 1), desc(i))):_*)
    lazy val level = properties("Level").toInt
    lazy val duration = properties("Duration") match {
        case "croche" => 0
        case "noire" => 1
        case "blanche" => 2
        case "ronde" => 3
        case "2xCroche" => 4
    }
    lazy val isNote = properties("Type") == "Note"
    lazy val pointed = properties("Pointed") == "*"
}

object Note {
    val propertiesDef = Array("Type", "Note", "Level", "Annotation", "Duration", "Pointed")
    //    val note1 = new Note("1 Cle Sol")
    //    val note2 = Note()
    val durationClass = 5
    
    def fromFile(filename: String) = {
        val lines = Source.fromFile(filename).getLines
        new Note(lines.next)
    }
}
