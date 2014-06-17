package gui

import com.intellij.uiDesigner.core.GridConstraints
import com.intellij.uiDesigner.core.GridLayoutManager
import javax.swing._
import java.awt._
import java.awt.event.ActionEvent
import impl.EasyUI
import impl.EasyUI.easyActionListener
import core.Note

/**
 * Created by guillaume on 21/05/14.
 */
object PreviewWindowScala extends App{

    final val NOTE_INDEX: Int = 0
    final val CLE_INDEX: Int = 1
    final val SILENCE_INDEX: Int = 2
    final val ANNOTATION_INDEX: Int = 3

    private def getDescription: String = {
        new Note(Array[String]( typeComboBox.getSelectedItem.toString,
                        noteComboBox1.getSelectedItem.toString,
                        levelSpinner.getValue.toString,
                        noteComboBox2.getSelectedItem.toString,
                        durationComboBox.getSelectedItem.toString,
                        if (pointedCheckBox.isSelected) "*" else "_"
                    )).toString
    }

    private def loadFile {
        this.imageChooser.showOpenDialog(this.loadButton)
        val imageFile = this.imageChooser.getSelectedFile
        if (imageFile != null) {
            imageNameTextArea.setText(imageFile.getName)
            imagePanel.load(imageFile)
        }
    }

    val previewWindow = new JPanel
    previewWindow.setLayout(new GridLayoutManager(4, 1, new Insets(0, 0, 0, 0), -1, -1))
    previewWindow.setMinimumSize(new Dimension(250, 250))
    
    val imageChooser = EasyUI.imageFileChooser()

    private val toolBar1: JToolBar = new JToolBar
    previewWindow.add(toolBar1, new GridConstraints(0, 0, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_WANT_GROW, GridConstraints.SIZEPOLICY_FIXED, null, new Dimension(-1, 20), null, 0, false))
    
    val imageNameTextArea = new JTextArea
    toolBar1.add(imageNameTextArea)
    
    val loadButton = new JButton
    loadButton.setText("Load")
    toolBar1.add(loadButton)
    loadButton.addActionListener((e: ActionEvent) => loadFile)
    
    val runButton = new JButton
    runButton.setText("Run")
    toolBar1.add(runButton)
    
    val infoLabel = new JLabel
    infoLabel.setText("Label")
    previewWindow.add(infoLabel, new GridConstraints(2, 0, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_FIXED, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false))

    val imagePanel = new ImagePanel( () => () )
    imagePanel.addInfoLabel(infoLabel)
    previewWindow.add(imagePanel, new GridConstraints(1, 0, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_BOTH, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_WANT_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_WANT_GROW, null, null, null, 0, false))
    
    val toolBar2: JToolBar = new JToolBar
    previewWindow.add(toolBar2, new GridConstraints(3, 0, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_WANT_GROW, GridConstraints.SIZEPOLICY_FIXED, null, new Dimension(-1, 20), null, 0, false))
    
    val typeComboBox = new JComboBox(Note.catDescription)
    toolBar2.add(typeComboBox)
    val noteComboBox1 = new JComboBox(Note.noteDescription)
    toolBar2.add(noteComboBox1)
    val levelSpinner = new JSpinner
    toolBar2.add(levelSpinner)
    val noteComboBox2 = new JComboBox(Note.annotationDescription)
    noteComboBox2.setMaximumSize(new Dimension(150, 500))
    toolBar2.add(noteComboBox2)
    val durationComboBox = new JComboBox(Note.durationDescription)
    durationComboBox.setEnabled(true)
    toolBar2.add(durationComboBox)
    val pointedCheckBox = new JCheckBox
    pointedCheckBox.setText("pointee")
    toolBar2.add(pointedCheckBox)
    
    val fileNumberTextField = new JFormattedTextField
    fileNumberTextField.setValue(new Integer(1))
    fileNumberTextField.setHorizontalAlignment(4)
    fileNumberTextField.setMinimumSize(new Dimension(150, 19))
    fileNumberTextField.setText("0")
    fileNumberTextField.setToolTipText("file number")
    toolBar2.add(fileNumberTextField)

    val okButton = new JButton
    okButton.setText("Ok")
    okButton.addActionListener((e: ActionEvent) => {
        val n: Int = fileNumberTextField.getValue.asInstanceOf[Integer]
        if (imagePanel.saveSelected("img_" + n, getDescription)) 
            fileNumberTextField.setValue(n + 1)
    })
    toolBar2.add(okButton)


    val frame: JFrame = new JFrame("PreviewWindow")
    frame.setContentPane(previewWindow)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.pack
    frame.setVisible(true)
}




