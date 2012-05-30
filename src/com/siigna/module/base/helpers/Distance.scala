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

/* 2010 (C) Copyright by Siigna, all rights reserved. */

import com.siigna.module.Module

import com.siigna._

object Distance extends Module {

  lazy val eventHandler = new EventHandler(stateMap, stateMachine)

  var points = List[Vector2D]()
  var p1 : Option[Vector2D] = None
  var p2 : Option[Vector2D] = None


  lazy val stateMap = DirectedGraph(
    'Start       -> 'MouseDown  -> 'FirstPoint,
    'FirstPoint  -> 'MouseUp   -> 'SecondPoint,
    'SecondPoint -> 'KeyEscape   -> 'Draw,
    'SecondPoint -> 'MouseUp  -> 'Draw,
    'Start       -> 'KeyEscape   -> 'Draw,
    'Draw        -> 'MouseMove  -> 'Buffer,
    'Draw        -> 'MouseUp  -> 'Buffer,
    'Draw        -> 'KeyEscape  -> 'Buffer,
    'Buffer      -> 'MouseUp  -> 'End,
    'Buffer      -> 'KeyEscape  -> 'End
  )

  lazy val stateMachine = Map(
    'FirstPoint -> ((events : List[Event]) => {
      events match {
        case MouseDown(point, MouseButtonLeft, _) :: tail => p1 = Some(point)
        case MouseUp(point, MouseButtonRight, _) :: tail => Goto ('End)
        case _ =>
      }
    }),

    'SecondPoint -> ((events : List[Event]) => {
      events match {
        case MouseDown(point, MouseButtonLeft, _) :: tail => {
         p2 = Some(point)
        }
        case _ =>
      }
    }),

    'Draw -> ((events : List[Event]) => {
      if (p1.isDefined && p2.isDefined) {
        Siigna.display("distance:  " + ((p2.get-p1.get).length.round) + " millimeters")
      }
    }),

    'Buffer -> ((events : List[Event]) => {
      events match {
        case MouseMove(point ,_,_) :: tail  =>  {
          if ((mousePosition - p2.get).length > 20)
             Goto ('End)
        }
        case _=>
      }
    }),
    'End -> ((events : List[Event]) => {
   })
 )
}
