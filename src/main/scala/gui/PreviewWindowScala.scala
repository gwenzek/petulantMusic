package gui

import com.intellij.uiDesigner.core.GridConstraints
import com.intellij.uiDesigner.core.GridLayoutManager
import javax.swing._
import java.awt._
import java.awt.event.ActionEvent
import impl.EasyUI
import impl.EasyUI.easyActionListener
import impl.EasyIO.FileCounter
import core.Note

/**
 * Created by guillaume on 21/05/14.
 */
object PreviewWindowScala extends App{

    private def loadFile {
        this.imageChooser.showOpenDialog(this.loadButton)
        val imageFile = this.imageChooser.getSelectedFile
        if (imageFile != null) {
            imageNameTextArea.setText(imageFile.getName)
            imagePanel.load(imageFile)
        }
    }
    val counter = new FileCounter("duration")
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

    val imagePanel = new ImagePanel()
    imagePanel.addInfoLabel(infoLabel)
    previewWindow.add(imagePanel, new GridConstraints(1, 0, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_BOTH, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_WANT_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_WANT_GROW, null, null, null, 0, false))
    
    val noteToolBar = new NoteToolBar
    previewWindow.add(noteToolBar, new GridConstraints(3, 0, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_WANT_GROW, GridConstraints.SIZEPOLICY_FIXED, null, new Dimension(-1, 20), null, 0, false))
    
    noteToolBar.okButton.addActionListener((e: ActionEvent) => {
        if (imagePanel.saveSelected("img_" + counter.index, noteToolBar.getNote.toString)) 
            counter += 1
    })

    val frame: JFrame = new JFrame("PreviewWindow")
    frame.setContentPane(previewWindow)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.pack
    frame.setVisible(true)
}




