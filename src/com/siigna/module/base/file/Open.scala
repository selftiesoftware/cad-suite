package com.siigna.module.base.file

/* 2010 (C) Copyright by Siigna, all rights reserved. */

//java
import java.awt.{FileDialog, Frame}
import java.io.File

//siigna
import com.siigna._

object Open extends Module {

  //val fromDatabase = new LineShape(p1,p2, _)

  lazy val eventHandler = EventHandler(stateMap, stateMachine)

  lazy val stateMap     = DirectedGraph('Start     -> 'KeyEscape -> 'End)

  lazy val stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      try {

        //val shapes : Iterable[Shape] = lines
        Siigna display "Loading not possible yet."

        Goto('End)

        // Create the shapes
        //Create(shapes)
      } catch {
        case e => {
          Siigna display "Open cancelled."
          Goto('End)
        }
      }
    }),
    'End   -> ((events : List[Event]) => { None })
  )
}