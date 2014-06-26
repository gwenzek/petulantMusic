package gui

import java.awt._
import java.awt.event.ActionEvent
import java.io.{IOException, File}
import javax.swing._
import com.intellij.uiDesigner.core.{GridConstraints, GridLayoutManager}
import com.sksamuel.scrimage.Image
import impl.EasyUI.{easyActionListener, gridLayout}
import core.{Pipeline, Note}
import core.NoteFinder.NoteBlock

/**
 * Created by guillaume on 14/06/14.
 */
object TestWindowScala extends App {

    val testWindow = new JPanel
    testWindow.setLayout(gridLayout(3, 2))

    // val pipeline = Pipeline.fromFile("layers/note_80.txt", "layers/duration_190.txt")
    val pipeline = Pipeline.empty

    private val panel1: JPanel = new JPanel
    panel1.setLayout(gridLayout(1, 1))
    testWindow.add(panel1, new GridConstraints(1, 1, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_BOTH, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, null, null, null, 0, false))
    
    private val zoomLabel = new JLabel
    val zoomWidth = 60
    val zoomHeight = 210
    zoomLabel.setText("zoom")
    zoomLabel.setBackground(new Color(0xFF0000))
    zoomLabel.setSize(zoomWidth, zoomHeight)
    panel1.add(zoomLabel, new GridConstraints(0, 0, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_BOTH, 1, 1, null, null, null, 0, false))
    
    val noteToolBar = new NoteToolBar
    testWindow.add(noteToolBar, new GridConstraints(2, 0, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_WANT_GROW, GridConstraints.SIZEPOLICY_FIXED, null, new Dimension(-1, 20), null, 0, false))
    
    noteToolBar.okButton.addActionListener((e: ActionEvent) => accept) 

    private val panel3: JPanel = new JPanel
    panel3.setLayout(gridLayout(1, 1))
    testWindow.add(panel3, new GridConstraints(1, 0, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_BOTH, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, null, new Dimension(400, 400), null, 0, false))
    
    val imagePanel = new ImagePanel(selectNote)
    private val imageScrollPane = new JScrollPane(imagePanel, ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS)
    panel3.add(imageScrollPane, new GridConstraints(0, 0, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_BOTH, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_WANT_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_WANT_GROW, null, null, null, 0, false))

    private val toolBar1: LoadToolBar = new LoadToolBar((f: File) => imagePanel.load(f))
    testWindow.add(toolBar1, new GridConstraints(0, 0, 1, 2, GridConstraints.ANCHOR_EAST, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_WANT_GROW, GridConstraints.SIZEPOLICY_FIXED, null, new Dimension(-1, 20), null, 0, false))
    
    toolBar1.trainButton.addActionListener((e: ActionEvent) => pipeline.train)

    val frame: JFrame = new JFrame("TestWindow")
    frame.setContentPane(testWindow)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.pack()
    frame.setVisible(true)
  

    def selectNote() : Unit = {
        val selected = new NoteBlock(Image(imagePanel.getSelectedImage), 0, 0, 0)
        val y = pipeline.getPrediction(selected)
        zoomLabel.setIcon(new ImageIcon(pipeline.getCurrentImage(zoomWidth, zoomHeight)))
        zoomLabel.repaint()
        noteToolBar.setNote(y)
    }

    def accept() : Unit = {
        val note = noteToolBar.getNote
        val message = {
            try { pipeline.accept(note); "saved"
            } catch {
              case e: IOException => "saving failed"
            }
        }
    }

}