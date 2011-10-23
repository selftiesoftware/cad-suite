/* 2010 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

import com.siigna.module.Module

import com.siigna._

object Group extends Module {

  lazy val eventHandler = new EventHandler(stateMap, stateMachine)

  lazy val stateMap = DirectedGraph(
    'Start     -> 'KeyEscape  -> 'End
  )

  lazy val stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      events match {
        case MouseDown(point, MouseButtonLeft, _) :: tail => ForwardTo('Select)
        case MouseDrag(point, MouseButtonRight, _) :: tail => ForwardTo('Select)
        case _ =>
      }
    }),

    'End -> ((events : List[Event]) => {
   })
 )
}
