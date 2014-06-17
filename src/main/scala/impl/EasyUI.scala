package impl

import scala.language.implicitConversions
import java.awt.event.{ActionEvent, ActionListener}
import java.io.File
import javax.swing.JFileChooser
import javax.swing.filechooser.FileFilter


object EasyUI{
    implicit def easyActionListener( f: ActionEvent => Unit ) = 
        new ActionListener() {
            override def actionPerformed(e: ActionEvent) = f(e)
        }

    implicit def easyFileFilter( filters: (String => Boolean)* ) : FileFilter = {
        def recAccept(s: String) : Boolean = {
            for(f <- filters){
                if (f(s)) return true
            }
            false
        }
        new FileFilter() {
            override def accept(file: File) = recAccept(file.getName)
            override val getDescription = s"Filtering according to ${filters.length} filters"
        }
    }

    def imageFileChooser() = {
        val chooser = new JFileChooser()
        chooser.setFileFilter(easyFileFilter(_.endsWith(".png"), _.endsWith(".jpeg"), _.endsWith(".gif")))
        chooser
    }
}