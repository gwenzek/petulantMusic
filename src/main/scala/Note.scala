/**
 * Created by Guillaume on 03/06/2014.
 */
class Note(description: String) {
    private val desc = description.split(" ")
    val index = desc(0).toInt
    val properties: Map[String, String] = Map[String, String]() ++ (for (i <- 1 until desc.length) yield (Note.propertiesDef(i - 1), desc(i)))
    lazy val level = properties("Level").toInt
    lazy val duration = properties("Duration") match {
        case "croche" => 0
        case "noire" => 1
        case "blanche" => 2
        case "ronde" => 3
    }
    lazy val isNote = properties("Type") == "Note"
}

object Note {
    val propertiesDef = Array("Type", "Note", "Level", "Annotation", "Duration", "Pointed")
    //    val note1 = new Note("1 Cle Sol")
    //    val note2 = Note()
    val durationClass = 4
}
