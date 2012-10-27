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

/*package com.siigna.module.base.create

import com.siigna.module.Module

import com.siigna._

object Group extends Module {

  lazy val stateMap = Map(
    'StartCategory -> ((events : List[Event]) => {
      events match {
        case MouseDown(point, MouseButtonLeft, _) :: tail => Module('Select)
        case MouseDrag(point, MouseButtonRight, _) :: tail => Module('Select)
        case _ =>
      }
      'End
    }),

    'End -> ()
  )
}
*/