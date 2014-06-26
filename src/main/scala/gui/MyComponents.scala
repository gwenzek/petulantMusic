package gui

import java.awt._
import java.awt.event.ActionEvent
import javax.swing._
import java.io.File
import com.intellij.uiDesigner.core.GridConstraints
import com.intellij.uiDesigner.core.GridLayoutManager
import com.sksamuel.scrimage.{Image, X11Colorlist}
import core.Symbol
import impl.EasyUI._
import impl.EasyIO.WriteAndClose


class NoteToolBar extends JToolBar {
    
    val typeComboBox = new JComboBox[String](Symbol.catDescription)
    this.add(typeComboBox)
    val noteComboBox1 = new JComboBox[String](Symbol.noteDescription)
    this.add(noteComboBox1)
    val levelSpinner = new JSpinner
    this.add(levelSpinner)
    val annontationComboBox = new JComboBox[String](Symbol.annotationDescription)
    this.add(annontationComboBox)
    val durationComboBox = new JComboBox[String](Symbol.durationDescription)
    this.add(durationComboBox)
    val pointedCheckBox = new JCheckBox
    pointedCheckBox.setText("pointee")
    this.add(pointedCheckBox)

    val okButton = new JButton
    okButton.setText("Ok")
    this.add(okButton)

    def getNote: Symbol = Symbol.fromArray(getContentAsArray)

    private def getContentAsArray: Array[String] = {
        Array[String]( 
            getContent(typeComboBox),
            getContent(noteComboBox1),
            if(getContent(levelSpinner) < 0) "_" else getContent(levelSpinner).toString,
            getContent(annontationComboBox),
            getContent(durationComboBox),
            if (getContent(pointedCheckBox)) "*" else "_"
        )
    } 

    def setNote(n: Symbol): Unit = {
        println(n.toString)
        val properties = n.toString.split(";")
        setContent(typeComboBox, properties(0))
        setContent(noteComboBox1, properties(1))
        setContent(levelSpinner, if(properties(2) == "_") -1 else properties(2).toInt)
        setContent(annontationComboBox, properties(3))
        setContent(durationComboBox, properties(4))
        setContent(pointedCheckBox, properties(5) == "*")
        this.repaint()
    }

    def dumpSelected(prefix: String) = (prefix + ".txt") <<| getNote.toString
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


import core.NoteFinder.NoteBlock

class ImagesNavigator(val onNew : NoteBlock => Unit) extends JPanel {
    var it = Iterator[NoteBlock]()

    this.setLayout(gridLayout(2, 1))

    val zoomPanel = new ZoomPanel(100, 200)
    this.add(zoomPanel, FillBoth(0, 0))
    val toolBar = new JToolBar

    this.add(toolBar, Horizontal(30)(1, 0))
    val nextButton = new JButton
    nextButton.setText("Next")
    nextButton.addActionListener((e: ActionEvent) => loadNext)
    toolBar.add(nextButton)

    var current = zoomPanel.default

    def loadNext : Unit = {
        if(it.hasNext){
            val note = it.next
            current = note.img
            onNew(note)
        } else {
            current = zoomPanel.default
        }
        zoomPanel.load(current)
    }

    def loadNotes(notes : Iterable[NoteBlock]){ it = notes.toIterator }

    def dumpSelected(prefix: String) = current.write(new File(prefix + ".png"))
}


class ZoomPanel(val w: Int, val h: Int) extends JPanel {
    this.setSize(w, h)
    private var img = default

    def default = Image.filled(w, h, X11Colorlist.Blue)

    def load(img: Image){
        this.img = img.fit(w, h)
        this.repaint()
    }

    override def paintComponent(g : Graphics) {
        this.setSize(w, h)
        super.paintComponent(g)
        g.drawImage(img.awt, 0, 0, null)
    }
}