package impl

import scala.language.implicitConversions
import java.awt.event.{ActionEvent, ActionListener}


object ListenerConversion{
    implicit def performedToListener( f: ActionEvent => Unit ) = 
        new ActionListener() {
            override def actionPerformed(e: ActionEvent) = f(e)
        }
}