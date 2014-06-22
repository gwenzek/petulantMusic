package impl

import scala.language.implicitConversions
import java.awt.event.{ActionEvent, ActionListener}
import java.awt.Insets
import java.io.File
import javax.swing.JFileChooser
import javax.swing.filechooser.FileFilter
import com.intellij.uiDesigner.core.GridLayoutManager

object EasyUI{
    implicit def easyActionListener( f: ActionEvent => Unit ) = 
        new ActionListener() {
            override def actionPerformed(e: ActionEvent) = f(e)
        }

    def gridLayout(x: Int, y: Int) = new GridLayoutManager(x, y, new Insets(0, 0, 0, 0), -1, -1)

    def easyFileFilter(desc: String,filters: (String => Boolean)* ) = new FileFilter() {
        override def accept(file: File) : Boolean = {
            if (file.isDirectory) return true
            for(f <- filters){
                if (f(file.getName)) return true
            }
            false
        }
        override val getDescription = desc
    }

    private def extToFilter(ext: String) : (String => Boolean) = _.endsWith(ext)

    def fileFilterByExt(summary: String, exts: String*) : FileFilter = {
        val desc = summary + '|' + exts.toString + '|'
        val filters = exts.map(extToFilter) 
        easyFileFilter(desc, filters:_*)
    }

    def imageFileChooser() = {
        val chooser = new JFileChooser()
        chooser.setFileFilter(fileFilterByExt("images", ".jpeg", ".gif", ".png"))
        chooser
    }
}