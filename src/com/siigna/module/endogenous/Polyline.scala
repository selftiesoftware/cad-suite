/* 2009 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

import com.siigna._

object Polyline extends Module {

  val eventHandler = EventHandler(stateMap, stateMachine)
  var points   = List[Vector2D]()
  var shape : PolylineShape = PolylineShape.empty

  override def paint(g : Graphics, t : TransformationMatrix) {
    if (shape.shapes.size > 0) g draw shape.transform(t)
  }

  def stateMap = DirectedGraph(
    'Start        -> 'KeyEscape  -> 'End,
    'Start        -> 'KeyDown    -> 'End
  )

  def stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      events match {
        case MouseUp(_, MouseButtonRight, _) :: tail => Goto('End)
        case MouseMove(point, _, _):: tail => {
          println("mouse position:" +point)
          Goto('Start)
        }
      }
    }),
    'End -> ((events : List[Event]) => (
      events match {
        case _ => None
      })
    )
  )
}