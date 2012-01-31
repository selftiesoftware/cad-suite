/*
 * Copyright (c) 2012. Siigna is released under the creative common license by-nc-sa. You are free
 * to Share — to copy, distribute and transmit the work,
 * to Remix — to adapt the work
 *
 * Under the following conditions:
 * Attribution —  You must attribute the work to http://siigna.com in the manner specified by the author or licensor (but not in any way that suggests that they endorse you or your use of the work).
 * Noncommercial — You may not use this work for commercial purposes.
 * Share Alike — If you alter, transform, or build upon this work, you may distribute the resulting work only under the same or similar license to this one.
 */

package com.siigna.module.base.file

/* 2010 (C) Copyright by Siigna, all rights reserved. */

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

        Siigna display "Loading file... Please wait."

        val sections : List[DXFSection] = sanitize(file)

        val shapes : List[Shape] = sections.map(_.toShape.getOrElse(None)).filterNot(_ == None).asInstanceOf[List[Shape]]

        Siigna display "Loading completed."

        Goto('End)
        frame.dispose() // Dispose of the frame so the thread can close down.
        Create(shapes) // Create the shapes
      } catch {
        case e => {
          Siigna display "Import cancelled."
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