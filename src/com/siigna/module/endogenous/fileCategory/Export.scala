package com.siigna.module.endogenous.fileCategory

/* 2010 (C) Copyright by Siigna, all rights reserved. */

import java.awt.{FileDialog, Frame}
import java.io.{BufferedWriter, FileWriter}

import com.siigna._
import com.siigna.util.dxf._

object Export extends Module {

  lazy val eventHandler = EventHandler(stateMap, stateMachine)

  lazy val stateMap     = DirectedGraph('Start     -> 'KeyEscape -> 'End)

  lazy val stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      try {
        val frame  = new Frame()
        val dialog = new FileDialog(frame, "Export to file", FileDialog.SAVE)
        dialog.setVisible(true)

        val directory = dialog.getDirectory
        val filename  = dialog.getFile
        val filetype  = filename.substring(filename.lastIndexOf('.')+1, filename.length());

        filetype match {
          case "dxf" => exportToDXF(filename, directory)
          case ""    => {
              exportToDXF(filename+".dxf", directory) // TODO: Change default?
            Siigna display "No fileextension found. Exporting DXF as default to "+filename+".dxf."
          }
          case t => Siigna display "Unsupported file extension. Export cancelled."
        }

        dialog.dispose
        frame.dispose

      } catch {
        case e => Siigna display "Export cancelled."
      }
      Goto('End)
      None
    }),
    'End   -> ((events : List[Event]) => { None })
  )

  def exportToDXF(filename : String, directory : String) = {
    val dxf = new DXFFile
    val writer = new FileWriter(directory+filename)
    val file   = new BufferedWriter(writer)
    dxf ++ Model.map(_.toDXF)
    file.write(dxf.toString)
    file.flush
    file.close
  }

}