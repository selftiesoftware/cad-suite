/* 2011 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

import com.siigna._

class Circle extends Module {

  val eventHandler = EventHandler(stateMap, stateMachine)

  var firstPoint : Option[Vector] = None

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
        case Message(p : PointShape) :: tail => {
          firstPoint = Some(p.point)
          ForwardTo('Point)
          Message(PointGuide(CircleShape(p.point, _)))
        }
        case _ =>
      }
      None
    }),
    'End -> ((events : List[Event]) => {
      events match {
        case Message(p : PointShape) :: tail => {
          Create(CircleShape(firstPoint.get, p.point))
        }
        case _ => None
      }
    })
  )

}