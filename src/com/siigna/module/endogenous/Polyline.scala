/* 2012 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

import com.siigna._

object Polyline extends Module {

  val eventHandler = EventHandler(stateMap, stateMachine)
  var points   = List[Vector2D]()
  var currentMouse = Vector2D(0,0)
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
            shape = PolylineShape.fromPoints(points)
          }
        }
        case MouseMove(position, _, _):: tail => {
          //store the current mouse position in a var
          currentMouse = position
          None
        }
        case _ =>
      }
    }),
    'End -> ((events : List[Event]) => (
      events match {
        case _ =>
          //create the final polyline
          Create(shape)

          //clear the points list
          points = List[Vector2D]()
      })
    )
  )

  override def paint(g : Graphics, t : TransformationMatrix) {
    if (points.length > 0) {
      g draw shape.transform(t)
      g draw LineShape(currentMouse,points.last).transform(t)
    }
  }
}