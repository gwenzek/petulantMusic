package core
/**
 * Created by Guillaume on 03/06/2014.
 */

import scala.io.Source


class Note(val properties: Array[String]) {

    def this(s: String) = this(s.split(";"))
  
    lazy val cat = Note.catDescription.indexOf(properties(Note.CAT))
    lazy val note = Note.noteDescription.indexOf(properties(Note.NOTE))
    lazy val level = properties(Note.LEVEL).toInt
    lazy val annotation = Note.annotationDescription.indexOf(properties(Note.ANNOTATION))
    lazy val duration = Note.durationDescription.indexOf(properties(Note.DURATION))
    
    lazy val isPointed = properties(Note.POINTED) == "*"
    lazy val isNote = cat == Note.catDescription.indexOf("Note")
    lazy val isSomething = if(cat == Note.catDescription.indexOf("Nothing")) 0 else 1

    override def toString() = {

        def concatAndIgnore(a: Array[String], ignore: Array[Int]) = {
            var res = a(0)
            for(i <- 1 until a.length){
                if(ignore.contains(i)) res += ";_"
                else res += ';' + a(i)
            }
            res
        }

        Note.catDescription(cat) match {
            case "Nothing" => concatAndIgnore(properties, Array(1, 2, 3, 4, 5))
            case "Note" => concatAndIgnore(properties, Array(-1))
            case "Cle" => concatAndIgnore(properties, Array(2, 3, 4, 5))
            case "Silence" => concatAndIgnore(properties, Array(1, 2, 3, 5))
            case "Annotation" => concatAndIgnore(properties, Array(1, 2, 4, 5))
        }
    }
}

object Note {
    val propertiesDef = Array("Cat", "Note", "Level", "Annotation", "Duration", "Pointed")
    final val CAT = propertiesDef.indexOf("Cat")
    final val NOTE = propertiesDef.indexOf("Note")
    final val LEVEL = propertiesDef.indexOf("Level")
    final val ANNOTATION = propertiesDef.indexOf("Annotation")
    final val DURATION = propertiesDef.indexOf("Duration")
    final val POINTED = propertiesDef.indexOf("Pointed")

    val catDescription = Array("Nothing", "Note", "Cle", "Silence", "Annotation")
    val noteDescription = Array("Do", "Re", "Mi", "Fa", "Sol", "La", "Si")
    val annotationDescription = Array("normal", "diese", "bemol", "becarre")
    val durationDescription = Array("2xCroche", "croche", "noire", "blanche", "ronde")

    def fromFile(filename: String) = {
        val lines = Source.fromFile(filename).getLines
        new Note(lines.next)
    }

    def partial(iLabel: (Int, String)*) = {
        val desc = new Array[String](propertiesDef.length)
        for((i, label) <- iLabel) desc(i) = label
        new Note(desc)
    }
}
