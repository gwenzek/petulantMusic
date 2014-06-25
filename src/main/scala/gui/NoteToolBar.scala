package gui

import java.awt._
import java.awt.event.ActionEvent
import javax.swing._
import java.io.File
import com.intellij.uiDesigner.core.GridConstraints
import com.intellij.uiDesigner.core.GridLayoutManager
import core.Symbol
import impl.EasyUI.{setContent, easyActionListener}


class NoteToolBar extends JToolBar {
    
    val typeComboBox = new JComboBox[String](Symbol.catDescription)
    this.add(typeComboBox)
    val noteComboBox1 = new JComboBox[String](Symbol.noteDescription)
    noteComboBox1.setEnabled(true)
    this.add(noteComboBox1)
    val levelSpinner = new JSpinner
    levelSpinner.setEnabled(true)
    this.add(levelSpinner)
    val noteComboBox2 = new JComboBox[String](Symbol.annotationDescription)
    noteComboBox2.setEnabled(true)
    this.add(noteComboBox2)
    val durationComboBox = new JComboBox[String](Symbol.durationDescription)
    this.add(durationComboBox)
    val pointedCheckBox = new JCheckBox
    pointedCheckBox.setText("pointee")
    pointedCheckBox.setEnabled(true)
    this.add(pointedCheckBox)

    val okButton = new JButton
    okButton.setText("Ok")
    this.add(okButton)

    def getNote: Symbol = {
        def getContent(box: JComboBox[String]) = 
            if (box.isEnabled) box.getSelectedItem.toString
            else "_"

        Symbol.fromArray(Array[String]( 
            getContent(typeComboBox),
            getContent(noteComboBox1),
            if(levelSpinner.getValue.toString.toInt < 0) "_" else levelSpinner.getValue.toString,
            getContent(noteComboBox2),
            getContent(durationComboBox),
            if (pointedCheckBox.isSelected && pointedCheckBox.isEnabled) "*" else "_"
        ))
    }

    def setNote(n: Symbol): Unit = {
        val properties = n.toString.split(";")
        setContent(typeComboBox, properties(0))
        setContent(noteComboBox1, properties(1))
        setContent(levelSpinner, if(properties(2) == "_") -1 else properties(2).toInt)
        setContent(noteComboBox2, properties(3))
        setContent(pointedCheckBox, properties(4) == "*")
        this.repaint()
    }


}

class LoadToolBar(val onLoad: File => Unit, val onTrain: () => Unit = () => ()) extends JToolBar {
    
    val imageName = new JLabel
    imageName.setHorizontalAlignment(10)
    imageName.setMaximumSize(new Dimension(1000, 18))
    imageName.setText("no image loaded")
    this.add(imageName)
    
    val loadButton = new JButton
    loadButton.setHorizontalAlignment(11)
    loadButton.setText("Load")
    loadButton.addActionListener((e: ActionEvent) => loadFile)
    this.add(loadButton)
    
    val trainButton = new JButton
    trainButton.setHorizontalAlignment(11)
    trainButton.setText("Train")
    trainButton.addActionListener((e: ActionEvent) => onTrain())
    this.add(trainButton)

    val imageChooser = new JFileChooser

    def loadFile {
        this.imageChooser.showOpenDialog(this.loadButton)
        val imageFile = this.imageChooser.getSelectedFile
        if (imageFile != null) {
            imageName.setText(imageFile.getName)
            onLoad(imageFile)
        }
    }
}