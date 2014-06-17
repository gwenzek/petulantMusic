package core
/**
 * Created by Guillaume on 03/06/2014.
 */

import scala.io.Source


class Note(description: String) {
    private val desc = description.split(";")
    val index = desc(0).toInt
    val properties: Map[String, String] = Map((for (i <- 1 until desc.length) yield (Note.propertiesDef(i - 1), desc(i))):_*)
    lazy val level = properties("Level").toInt
    lazy val duration = Note.durationDescription.indexOf(properties("Duration"))
    lazy val isNote = properties("Type") == "Note"
    lazy val pointed = properties("Pointed") == "*"
}

object Note {
    val propertiesDef = Array("Type", "Note", "Level", "Annotation", "Duration", "Pointed")
    //    val note1 = new Note("1 Cle Sol")
    //    val note2 = Note()
    lazy val durationClass = durationDescription.length
    val durationDescription = Array("2xCroche", "croche", "noire", "blanche", "ronde")
    val typeDescription = Array("Note", "Cle", "Silence", "Annotation", "Nothing")

    def fromFile(filename: String) = {
        val lines = Source.fromFile(filename).getLines
        new Note(lines.next)
    }
}
