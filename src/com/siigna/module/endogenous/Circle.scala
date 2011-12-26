/* 2011 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

import com.siigna._

object Circle extends Module {

  val eventHandler = EventHandler(stateMap, stateMachine)

  var firstPoint : Option[Vector2D] = None

  def stateMap = DirectedGraph(
    'Start         -> 'MouseMove -> 'Start,
    'Start         -> 'Action    -> 'FirstPoint,
    'FirstPoint    -> 'Action    -> 'End
  )

  def stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      ForwardTo('Point)
      None
    }),
    'FirstPoint -> ((events : List[Event]) => {
      events match {
        case Message(p : Vector2D) :: tail => {
          firstPoint = Some(p)
          ForwardTo('Point)
          Message(PointGuide(CircleShape(p, _)))
        }
        case _ =>
      }
      None
    }),
    'End -> ((events : List[Event]) => {
      events match {
        case Message(p : Vector2D) :: tail => {
          Create(CircleShape(firstPoint.get, p))
        }
        case _ => None
      }
    })
  )

}