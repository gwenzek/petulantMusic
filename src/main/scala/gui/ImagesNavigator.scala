package gui

import com.sksamuel.scrimage.{Image, X11Colorlist}
import java.awt._
import java.awt.event.ActionEvent
import java.io.IOException
import javax.swing._
import impl.EasyUI._
import core.Pipeline
import java.io.File
import core.NoteFinder.NoteBlock


object Demo extends App {
    val mainWindow = new JPanel
    mainWindow.setLayout(gridLayout(3, 2))
    val pipeline = Pipeline.empty

    val imagePanel = new ImagePanel
    mainWindow.add(imagePanel, FillBoth(1, 0))
    val loadToolBar = new LoadToolBar(onLoad, pipeline.train)
    mainWindow.add(loadToolBar, Horizontal(30)(0, 0, 1, 2))
    val noteToolBar = new NoteToolBar 
    mainWindow.add(noteToolBar, Horizontal(30)(2, 0, 1, 2))

    var navigator = new ImagesNavigator(onNewNote)
    mainWindow.add(navigator, Vertical(100)(1, 1))

    def onLoad(imgFile: File) : Unit = {
        val img = Image(imgFile)
        val (imgWithTag, notes) = pipeline.loadImageAndSplit(img)
        imagePanel.load(imgWithTag)
        navigator.loadNotes(notes)
        mainWindow.add(navigator, Vertical(100)(1, 1))
    }

    def onNewNote(note: NoteBlock) : Unit = {
        val predicted = pipeline.getPrediction(note)
        noteToolBar.setNote(predicted)
    }

    val frame: JFrame = new JFrame("TestWindow")
    frame.setContentPane(mainWindow)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.pack()
    frame.setVisible(true)

}

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

    def loadNext : Unit = {
        println("next")
        if(it.hasNext){
            val note = it.next 
            zoomPanel.load(note.img)
            onNew(note)
        } else {
            zoomPanel.load(zoomPanel.default)
        }
    }

    def loadNotes(notes : Iterable[NoteBlock]){ it = notes.toIterator }
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