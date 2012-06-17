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

import fileformats.dxf.{DXFSection, DXFFile}
import java.awt.{FileDialog, Frame}
import java.io.{BufferedWriter, FileWriter}
import com.siigna._

object Export extends Module {

  lazy val eventHandler = EventHandler(stateMap, stateMachine)
  private var frameIsLoaded : Boolean = false
  lazy val stateMap     = DirectedGraph('Start     -> 'KeyEscape -> 'End)

  lazy val stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      //a hack to prevent the dialog from opening twice
      if (frameIsLoaded == false) {
        try {
          frameIsLoaded = true
          val frame  = new Frame()
          val dialog = new FileDialog(frame, "Export to file", FileDialog.SAVE)
          dialog.setVisible(true)

          val directory = dialog.getDirectory
          val filename  = dialog.getFile
          val filetype  = filename.substring(filename.lastIndexOf('.')+1, filename.length());

          filetype match {
            case "dxf" => exportToDXF(filename, directory)
            case ""    => {
              // TODO: Change default?
                exportToDXF(filename+".dxf", directory)
              Siigna display "No fileextension found. Exporting DXF as default to "+filename+".dxf."
            }
            case t => Siigna display "Unsupported file extension. Export cancelled."
          }

          dialog.dispose
          frame.dispose

        } catch {
          case e => Siigna display "Export cancelled."
        }
      }
      Goto('End)
      None
    }),
    'End   -> ((events : List[Event]) => {frameIsLoaded = false })
  )

  def exportToDXF(filename : String, directory : String) = {
    val dxf = new DXFFile
    val writer = new FileWriter(directory+filename)
    val file   = new BufferedWriter(writer)

    dxf ++ Model.map(t => DXFSection.toDXF(t._2)).toSeq

    file.write(dxf.toString)
    file.flush
    file.close
  }

}