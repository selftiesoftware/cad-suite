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
