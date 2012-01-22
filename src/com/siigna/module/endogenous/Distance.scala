/* 2010 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

import com.siigna.module.Module

import com.siigna._

object Distance extends Module {

  lazy val eventHandler = new EventHandler(stateMap, stateMachine)

  var points = List[Vector2D]()
  var p1 : Option[Vector2D] = None
  var p2 : Option[Vector2D] = None
  var mousePosition  = Vector2D(0, 0)


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
          mousePosition = point
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
