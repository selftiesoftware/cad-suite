/* 2009 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

import com.siigna._

object Polyline extends Module {

  val eventHandler = EventHandler(stateMap, stateMachine)
  var points   = List[Vector2D]()
  var shape : PolylineShape = PolylineShape.empty

  def stateMap = DirectedGraph(
    'Start        -> 'KeyEscape  -> 'End,
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
            println(shape)
            shape = PolylineShape.fromPoints(points)
          }
        }
        case MouseMove(position, _, _):: tail => {
          //draw a line from the last clicked point to the current mouse position
          None

        }
      }
    }),
    'End -> ((events : List[Event]) => (
      events match {
        case _ =>
          None
      })
    )
  )
  override def paint(g : Graphics, t : TransformationMatrix) {
    println("shape: " +shape)
    println("shapes: "+shape.shapes)
    if (shape.shapes.size > 0) g draw shape.transform(t)
  }
}