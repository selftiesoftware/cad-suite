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

package com.siigna.module.base.helpers

import com.siigna._

/**
 * Toggles snap on-off
 */

class SnapToggle extends Module {

  var isSnapping : Boolean = true

  val stateMap : StateMap = Map(

    'Start-> {
      case _ => {
        if (Snap.snapEnabled == true) {
          Snap.snapEnabled = false
          Siigna display ("snap is off")
          isSnapping = false
        }
        else {
          Snap.snapEnabled = true
          Siigna display ("snap is on")
        }
        End
      }
    }
  )
}