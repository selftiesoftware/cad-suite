/* 2010 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

//java
import java.awt.{FileDialog, Frame}
import java.io.File

//siigna
import com.siigna._

object Open extends Module {

  lazy val eventHandler = EventHandler(stateMap, stateMachine)

  lazy val stateMap     = DirectedGraph('Start     -> 'KeyEscape -> 'End)

  lazy val stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      try {
        //opens a file dialog
        //val frame = new Frame
        //val dialog = new FileDialog (frame)
        //dialog.setVisible(true)
        //val fileName = dialog.getFile
        //val fileDir = dialog.getDirectory
        //val file = new File(fileDir + fileName)

        //interface display "Loading file... Please wait."

        //val geom = scala.io.Source.fromFile(file).getLines
        //val line    = geom.next.trim

        //match
        //  shapes
        //  regulær pattern matching
        //val shapes : Iterable[Shape] = lines
        interface display "Loading not possible yet."

        Goto('End)
        //frame.dispose() // Dispose of the frame so the thread can close down.
        // Create the shapes
        //Create(shapes)
      } catch {
        case e => {
          interface display "Open cancelled."
          Goto('End)
        }
      }
    }),
    'End   -> ((events : List[Event]) => { None })
  )
}
