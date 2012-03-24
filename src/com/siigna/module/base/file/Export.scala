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
    dxf ++ Model.map(_._2.toDXF).seq.toSeq
    file.write(dxf.toString)
    file.flush
    file.close
  }

}