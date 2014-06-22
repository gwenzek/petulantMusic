package gui

import java.awt._
import javax.swing._
import com.intellij.uiDesigner.core.GridConstraints
import com.intellij.uiDesigner.core.GridLayoutManager
import core.Note


class NoteToolBar extends JToolBar {
    
    val typeComboBox = new JComboBox[String](Note.catDescription)
    this.add(typeComboBox)
    val noteComboBox1 = new JComboBox[String](Note.noteDescription)
    noteComboBox1.setEnabled(false)
    this.add(noteComboBox1)
    val levelSpinner = new JSpinner
    levelSpinner.setEnabled(false)
    this.add(levelSpinner)
    val noteComboBox2 = new JComboBox[String](Note.annotationDescription)
    noteComboBox2.setEnabled(false)
    this.add(noteComboBox2)
    val durationComboBox = new JComboBox[String](Note.durationDescription)
    this.add(durationComboBox)
    val pointedCheckBox = new JCheckBox
    pointedCheckBox.setText("pointee")
    pointedCheckBox.setEnabled(false)
    this.add(pointedCheckBox)

    val okButton = new JButton
    okButton.setText("Ok")
    this.add(okButton)

    def getNote: Note = {
        def getContent(box: JComboBox[String]) = 
            if (box.isEnabled) box.getSelectedItem.toString
            else "_"

        new Note(Array[String]( getContent(typeComboBox),
                        getContent(noteComboBox1),
                        "_", //levelSpinner.getValue.toString
                        getContent(noteComboBox2),
                        getContent(durationComboBox),
                        if (pointedCheckBox.isSelected && pointedCheckBox.isEnabled) "*" else "_"
                    ))
    }

    def setNote(n: Note): Unit = {
        def setContent(box: JComboBox[String], i: Int) = 
            if(box.isEnabled) box.setSelectedIndex(i)

        setContent(typeComboBox, n.cat)
        setContent(noteComboBox1, n.note)
        if(levelSpinner.isEnabled) levelSpinner.setValue(n.level)
        setContent(noteComboBox2, n.annotation)
        setContent(durationComboBox, n.duration)
        if(pointedCheckBox.isEnabled && n.isPointed) pointedCheckBox.setSelected(true)
    }


}