import java.awt._
import java.awt.event.ActionEvent
import javax.swing._

import com.intellij.uiDesigner.core.{GridConstraints, GridLayoutManager}
import com.sksamuel.scrimage.Image
import impl.ListenerConversion.performedToListener
import core.BinaryImage

/**
 * Created by guillaume on 14/06/14.
 */
object TestWindowScala extends App {
    
    val testWindow = new JPanel
    testWindow.setLayout(new GridLayoutManager(2, 2, new Insets(0, 0, 0, 0), -1, -1))

    private val panel1: JPanel = new JPanel
    panel1.setLayout(new GridLayoutManager(2, 1, new Insets(0, 0, 0, 0), -1, -1))
    testWindow.add(panel1, new GridConstraints(1, 1, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_BOTH, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, null, null, null, 0, false))
    
    private val zoomLabel = new JLabel
    zoomLabel.setText("zoom")
    zoomLabel.setBackground(new Color(0xFF0000))
    zoomLabel.setLayout(new GridLayoutManager(1, 1, new Insets(0, 0, 0, 0), -1, -1))
    panel1.add(zoomLabel, new GridConstraints(0, 0, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_BOTH, 1, 1, null, null, null, 0, false))
    
    private val panel2: JPanel = new JPanel
    panel2.setLayout(new GridLayoutManager(3, 2, new Insets(0, 0, 0, 0), -1, -1))
    panel1.add(panel2, new GridConstraints(1, 0, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_BOTH, GridConstraints.SIZEPOLICY_CAN_SHRINK, GridConstraints.SIZEPOLICY_CAN_SHRINK, null, null, null, 0, false))
    
    val durationComboBox = new JComboBox[String](Array("2xCroche", "Croche", "Noir", "Blanche", "Ronde"))
    panel2.add(durationComboBox, new GridConstraints(0, 1, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_FIXED, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false))
    
    val fileNumberField = new JFormattedTextField
    panel2.add(fileNumberField, new GridConstraints(1, 1, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_FIXED, GridConstraints.SIZEPOLICY_FIXED, null, new Dimension(150, -1), null, 0, false))
    
    private val label1: JLabel = new JLabel
    label1.setText("Duration")
    panel2.add(label1, new GridConstraints(0, 0, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_FIXED, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false))
    
    private val label2: JLabel = new JLabel
    label2.setText("File number")
    panel2.add(label2, new GridConstraints(1, 0, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_FIXED, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false))
    
    val OKButton = new JButton
    OKButton.setText("OK")
    panel2.add(OKButton, new GridConstraints(2, 1, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false))
    
    private val panel3: JPanel = new JPanel
    panel3.setLayout(new GridLayoutManager(1, 1, new Insets(0, 0, 0, 0), -1, -1))
    testWindow.add(panel3, new GridConstraints(1, 0, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_BOTH, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, null, new Dimension(356, 24), null, 0, false))
    
    val imagePanel = new ImagePanel(showZoom)
    private val imageScrollPane = new JScrollPane(imagePanel, ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS)
    imagePanel.addInfoLabel(new JLabel)
    panel3.add(imageScrollPane, new GridConstraints(0, 0, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_BOTH, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_WANT_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_WANT_GROW, null, null, null, 0, false))


    private val toolBar1: JToolBar = new JToolBar
    testWindow.add(toolBar1, new GridConstraints(0, 0, 1, 2, GridConstraints.ANCHOR_EAST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_WANT_GROW, GridConstraints.SIZEPOLICY_FIXED, null, new Dimension(-1, 20), null, 0, false))
    
    val imageName = new JLabel
    imageName.setHorizontalAlignment(10)
    imageName.setMaximumSize(new Dimension(1000, 18))
    imageName.setText("no image loaded")
    toolBar1.add(imageName)
    
    val loadButton = new JButton
    loadButton.setHorizontalAlignment(11)
    loadButton.setText("Load")
    loadButton.addActionListener((e: ActionEvent) => {
        loadFile
        imageScrollPane.repaint()
        })
    toolBar1.add(loadButton)
    
    val trainButton = new JButton
    trainButton.setHorizontalAlignment(11)
    trainButton.setText("Train")
    trainButton.addActionListener((e: ActionEvent) => showZoom)
    toolBar1.add(trainButton)

    val imageChooser = new JFileChooser

    val frame: JFrame = new JFrame("TestWindow")
    frame.setContentPane(testWindow)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.pack()
    frame.setVisible(true)

    def loadFile {
        this.imageChooser.showOpenDialog(this.loadButton);
        val imageFile = this.imageChooser.getSelectedFile();
        if (imageFile != null) {
            imageName.setText(imageFile.getName());
            imagePanel.load(imageFile);
        }
        imageScrollPane.repaint()
    }

    def showZoom() {
        val buffered = imagePanel.getSelectedImage
        val img = BinaryImage.centerOnLines(Image(buffered), 20, 70, 20, 50)
        zoomLabel.setIcon(new ImageIcon(buffered))
        zoomLabel.repaint()
    }

}
