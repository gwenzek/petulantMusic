package gui

import com.sksamuel.scrimage.Image
import java.awt._
import java.awt.event.ActionEvent
import java.io.File
import javax.swing._
import impl.EasyUI._
import impl.EasyIO.FileCounter
import core.Pipeline
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
    noteToolBar.okButton.addActionListener((e: ActionEvent) => saveImgAndNote)
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

    val counter = FileCounter("collected")
    def saveImgAndNote() = {
        navigator.dumpSelected(counter.getFile("img"))
        noteToolBar.dumpSelected(counter.getFile("img"))
        counter += 1
    }

    val frame: JFrame = new JFrame("TestWindow")
    frame.setContentPane(mainWindow)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.pack()
    frame.setVisible(true)

}