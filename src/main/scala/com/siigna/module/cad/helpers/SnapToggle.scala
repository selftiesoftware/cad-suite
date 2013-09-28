/*
 * Copyright (c) 2008-2013, Selftie Software. Siigna is released under the
 * creative common license by-nc-sa. You are free
 *   to Share — to copy, distribute and transmit the work,
 *   to Remix — to adapt the work
 *
 * Under the following conditions:
 *   Attribution —   You must attribute the work to http://siigna.com in
 *                    the manner specified by the author or licensor (but
 *                    not in any way that suggests that they endorse you
 *                    or your use of the work).
 *   Noncommercial — You may not use this work for commercial purposes.
 *   Share Alike   — If you alter, transform, or build upon this work, you
 *                    may distribute the resulting work only under the
 *                    same or similar license to this one.
 *
 * Read more at http://siigna.com and https://github.com/siigna/main
 */

package com.siigna.module.cad.helpers

import com.siigna._
import module.Tooltip

/**
 * Toggles snap on-off
 */

class SnapToggle extends Module {

  var isSnapping : Boolean = true

  val stateMap : StateMap = Map(

    'Start-> {
      case _ => {
        val value = Siigna.snapToggle match {
          case true => "on"
          case _ => "off"
        }
        Siigna display "Snap is " + value
        Tooltip.blockUpdate(3500)
        End
      }
    }
  )
}