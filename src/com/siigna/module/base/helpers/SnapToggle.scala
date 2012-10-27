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

/*package com.siigna.module.base.helpers
import com.siigna._
import app.view.event.Snap

/**
 * Toggles snap on-off
 */

object SnapToggle extends Module {

  val eventHandler = EventHandler(stateMap, stateMachine)

  var isSnapping : Boolean = true

  lazy val stateMap = DirectedGraph (
    'StartCategory    -> 'Event     -> 'End
  )

  lazy val stateMachine = Map(

    'StartCategory -> ((events : List[Event]) => {
      Goto('End)
      None
    }),

    'End -> ((events : List[Event]) => {
      if (Snap.snapEnabled == true) {
        Siigna display "snap is off"
        Snap.snapEnabled = false
      }
      else {
        Siigna display "snap is on"
        Snap.snapEnabled = true
      }
      None
    })
  )
}*/