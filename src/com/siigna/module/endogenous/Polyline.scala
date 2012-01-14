/* 2009 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

import com.siigna._

object Polyline extends Module {

  val eventHandler = EventHandler(stateMap, stateMachine)
  var points   = List[Vector2D]()
  var shape : PolylineShape = PolylineShape.empty

  def stateMap = DirectedGraph(
    'Start        -> 'KeyDown    -> 'End
  )

  def stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      events match {
        case MouseUp(_, MouseButtonRight, _) :: tail => Goto('End)
        case MouseDown(point, _, _):: tail => {
          points = points :+ point
          if (points.size > 0){
            //draw a polyline from the points saved in shape
            shape = PolylineShape.fromPoints(points)
          }
        }
        case  _ =>
      }
    }),
    'End -> ((events : List[Event]) => {
      points = Nil
      shape = PolylineShape.empty
    })
  )

  override def paint(g : Graphics, t : TransformationMatrix) {
    if (shape.shapes.size > 0)
      g draw shape.transform(t)

    if (points.size > 0)
      g draw LineShape(points.last, Siigna.mousePosition).transform(t)
  }
}