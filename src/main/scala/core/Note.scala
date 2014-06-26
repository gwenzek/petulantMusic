package core
/**
 * Created by Guillaume on 03/06/2014.
 */

import scala.io.Source


abstract class Symbol {
    override def toString() = this match {
        case Nothing => "Nothing;_;_;_;_;_"
        case n: Note => s"Note;${n.getNote};${n.getLevel};"+
            Symbol.annotationDescription(n.annotation) + ';' +
            Symbol.durationDescription(n.duration) + ';' +
            (if(n.pointed) '*' else '_')
        case c: Cle => "Cle;" + Symbol.noteDescription(c.note) + ";_;_;_;_"
        case s: Silence => "Silence;_;_;_;" +Symbol.durationDescription(s.duration)+";_"
        case a: Annotation => s"Annotation;${a.getNote};${a.getLevel};"
    }

    def simpleCat = this match {
        case Nothing => 0
        case Note(_, duration, _, _) => duration
        case Cle(_) => 6
        case Annotation(_, _) => 7
        case _ => 0
    }
   
    def isNote = this match {
        case n: Note => true
        case _ => false
    }
}

object Symbol {
    val propertiesDef = Array("Cat", "Note", "Level", "Annotation", "Duration", "Pointed")
    final val CAT = propertiesDef.indexOf("Cat")
    final val NOTE = propertiesDef.indexOf("Note")
    final val LEVEL = propertiesDef.indexOf("Level")
    final val ANNOTATION = propertiesDef.indexOf("Annotation")
    final val DURATION = propertiesDef.indexOf("Duration")
    final val POINTED = propertiesDef.indexOf("Pointed")

    val catDescription = Array("Nothing", "Note", "Cle", "Silence", "Annotation")
    val noteDescription = Array("Do", "Re", "Mi", "Fa", "Sol", "La", "Si", "_")
    val annotationDescription = Array("_", "normal", "diese", "bemol", "becarre")
    val durationDescription = Array("_", "2xCroche", "croche", "noire", "blanche", "ronde")

    def fromFile(filename: String) = {
        val lines = Source.fromFile(filename).getLines
        fromString(lines.next)
    }

    // def partial(iLabel: (Int, String)*) = {
    //     val desc = new Array[String](propertiesDef.length)
    //     for((i, label) <- iLabel) desc(i) = label
    //     new Symbol(desc)
    // }

    def fromSimpleCat(i: Int) = {
        if(i == 0) Nothing
        else if(i <= 5) Note(0, i, 0, false)
        else if(i == 6) Cle(noteDescription.indexOf("Sol"))
        else if(i == 7) Annotation(0, noteDescription.indexOf("diese"))
        else Nothing
    }

    val simpleCatNumber = 8
    def fromString(str: String) = fromArray(str.split(";"))
    def fromArray(properties: Array[String]) = {
        properties(CAT) match {
            case "Nothing" => Nothing
            case "Cle" => Cle(noteDescription.indexOf(properties(NOTE)))
            case "Silence" => Silence(durationDescription.indexOf(properties(DURATION)))
            case "Annotation" => 
                val level = levelFromString(properties(NOTE), properties(LEVEL))
                val annotation = annotationDescription.indexOf(properties(ANNOTATION))
                Annotation(level, annotation)
            case "Note" => 
                val level = levelFromString(properties(NOTE), properties(LEVEL))
                val duration = durationDescription.indexOf(properties(DURATION))
                val annotation = annotationDescription.indexOf(properties(ANNOTATION))
                val pointed = properties(POINTED) == "*"
                Note(level, duration, annotation, pointed)
        }
        
    }

    private def levelFromString(s1: String, s2: String) = {
        val i = noteDescription.indexOf(s1)
        if(s2 == "_") -1
        else i + 7*s2.toInt + 4       
    }

    def main(args: Array[String]) = {
        val desc = "Note;_;_;_;blanche;_"
        val n = Symbol.fromString(desc)
        println(n.toString)
    }
}

case object Nothing extends Symbol
case class Note(level: Int, duration: Int, annotation: Int, pointed: Boolean) extends Symbol {
    def getNote = if (level < 0) "_" else Symbol.noteDescription((level + 3) % 7)
    def getLevel = if (level < 0) "_" else (level + 3) / 7
}
case class Cle(note: Int) extends Symbol
case class Silence(duration: Int) extends Symbol
case class Annotation(level: Int, annotation: Int) extends Symbol {
    def getNote = Symbol.noteDescription((level - 4) % 7)
    def getLevel = (level - 4) / 7
}
