/* 2010 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

//java
import java.awt.{FileDialog, Frame}
import java.io.File

//siigna
import com.siigna._
import com.siigna.util.dxf._

object Import extends Module {

  lazy val eventHandler = EventHandler(stateMap, stateMachine)

  lazy val stateMap     = DirectedGraph('Start     -> 'KeyEscape -> 'End)

  lazy val stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      try {
        //opens a file dialog
        val frame = new Frame
        val dialog = new FileDialog (frame)
        dialog.setVisible(true)
        val fileName = dialog.getFile
        val fileDir = dialog.getDirectory
        val file = new File(fileDir + fileName)

        interface display "Loading file... Please wait."

        val sections : List[DXFSection] = sanitize(file)

        val shapes : List[Shape] = sections.map(_.toShape.getOrElse(None)).filterNot(_ == None).asInstanceOf[List[Shape]]

        interface display "Loading completed."

        Goto('End)
        frame.dispose() // Dispose of the frame so the thread can close down.
        Create(shapes) // Create the shapes
      } catch {
        case e => {
          interface display "Import cancelled."
          Goto('End)
        }
      }

    }),
    'End   -> ((events : List[Event]) => { None })
  )

  /**
   * Sanitizes a list of char from a file into a list of strings, representing
   * one line per string.
   */
  def sanitize(file : File) : List[DXFSection] = {
    var dxfValues   = DXFSection(List())
    var dxfSections = List[DXFSection]()
    val lines       = scala.io.Source.fromFile(file).getLines

    while (lines.hasNext) {
      // Get the values
      try {
        val id    = lines.next.trim
        val value = lines.next.trim

        // Add the DXF-value
        try {
          dxfValues += DXFValue(id.toInt, value)
        } catch {
          case e => Log.error("Import: Failed to convert id '"+id+"' to Integer in ("+id+" -> "+value+"). Line skipped.")
        }
      } catch {
        case e => Log.error("Import: Unspecified error encountered. Line skipped.")
      }
    }

    var index : Int = 0
    var sectionStartIndex : Option[Int] = None
    dxfValues.values.foreach((value : DXFValue) => {
      if (value.a == 0 && sectionStartIndex.isDefined && value.b != "VERTEX") { // Exclude the Rhino vertex, since we need it in the Polyline section.
        dxfSections = dxfSections :+ DXFSection(dxfValues.values.slice(sectionStartIndex.get, index))
        sectionStartIndex = Some(index)
      } else if (value.a == 0 && value.b != "VERTEX") {
        sectionStartIndex = Some(index)
      }
      index += 1
    })

    dxfSections
  }
}