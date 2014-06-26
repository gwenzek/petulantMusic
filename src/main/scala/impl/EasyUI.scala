package impl

import scala.language.implicitConversions
import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{Insets, Dimension}
import java.io.File
import javax.swing._
import javax.swing.filechooser.FileFilter
import com.intellij.uiDesigner.core.{GridConstraints, GridLayoutManager}
import com.intellij.uiDesigner.core.GridConstraints._


object EasyUI{
    implicit def easyActionListener( f: ActionEvent => Unit ) = 
        new ActionListener() {
            override def actionPerformed(e: ActionEvent) = f(e)
        }

    // implicit def easyActionListener( f: Unit ) = 
    //     new ActionListener() {
    //         override def actionPerformed(e: ActionEvent) = f
    //     }

    def gridLayout(x: Int, y: Int) = new GridLayoutManager(x, y, new Insets(0, 0, 0, 0), -1, -1)

    abstract class Layout{
        def anchor: Int
        def fill: Int
        def HSizePolicy: Int
        def VSizePolicy: Int
        def minimumSize: Dimension
        def maximumSize: Dimension
        def preferredSize: Dimension
        def apply(x: Int, y: Int, w: Int = 1, h: Int = 1) = 
            new GridConstraints(x, y, w, h, anchor, fill, HSizePolicy, VSizePolicy, minimumSize, maximumSize, preferredSize, 0, false)
    }

    case class Horizontal(val height: Int) extends Layout {
        val anchor = ANCHOR_CENTER
        val fill = FILL_HORIZONTAL
        val HSizePolicy = SIZEPOLICY_CAN_GROW | SIZEPOLICY_WANT_GROW
        val VSizePolicy = SIZEPOLICY_FIXED
        val minimumSize = new Dimension(-1, height)
        val maximumSize = new Dimension(-1, height)
        val preferredSize = new Dimension(-1, height)
    }

    case class Vertical(val width: Int) extends Layout {
        val anchor = ANCHOR_CENTER
        val fill = FILL_VERTICAL
        val HSizePolicy = SIZEPOLICY_FIXED
        val VSizePolicy = SIZEPOLICY_CAN_GROW | SIZEPOLICY_WANT_GROW
        val minimumSize = new Dimension(width, -1)
        val maximumSize = new Dimension(width, -1)
        val preferredSize = new Dimension(width, -1)
    }

    case object FillBoth extends Layout {
        val anchor = ANCHOR_CENTER
        val fill = FILL_BOTH
        val HSizePolicy = SIZEPOLICY_CAN_GROW | SIZEPOLICY_WANT_GROW
        val VSizePolicy = SIZEPOLICY_CAN_GROW | SIZEPOLICY_WANT_GROW
        val minimumSize = new Dimension(-1, -1)
        val maximumSize = new Dimension(-1, -1)
        val preferredSize = new Dimension(-1, -1)
    }

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

    def setContent(box: JComboBox[_], i: Int) : Unit = 
        box.setSelectedIndex(i)
    def setContent(box: JComboBox[String], s: String) : Unit = 
        box.setSelectedItem(s)
    def setContent(spinner: JSpinner, i: Int) : Unit = 
        spinner.setValue(i)
    def setContent(checkBox: JCheckBox, b : Boolean) : Unit = 
        checkBox.setSelected(b)

    def getContent(box: JComboBox[String]): String = box.getSelectedItem.toString
    def getContent(spinner: JSpinner): Int = spinner.getValue.toString.toInt
    def getContent(checkBox: JCheckBox): Boolean = checkBox.isSelected

}