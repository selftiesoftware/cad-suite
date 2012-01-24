/* 2012 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

import com.siigna._

object Polyline extends Module {

    // Store the mousePosition, so we get the snap-coordinates
  private var mousePosition : Option[Vector2D] = None

  // The points of the polyline
  private var points   = List[Vector2D]()

  private var previousPoint : Option[Vector2D] = None

   // The polylineshape so far
  private var shape : PolylineShape = PolylineShape.empty

  val eventHandler = EventHandler(stateMap, stateMachine)

  def stateMap = DirectedGraph(
    'Start        -> 'KeyEscape  -> 'End
  )

  def stateMachine = Map(

    'Start -> ((events : List[Event]) => {
      println("PL, events: "+events.head)
      events match {
        case _ => Goto('Point)
      }
    }),
    'Point -> ((events : List[Event]) => {
      events match {
        //if the point module returns a valid point, add this to the polyline.
        case Message(point : Vector2D) :: tail => {
          points = points :+ point
          ForwardTo('Point)
        }
        case MouseUp(position, MouseButtonLeft,_):: tail => ForwardTo('Point)
        case MouseDown(position, _,_):: tail => ForwardTo('Point)
        case MouseUp(_, MouseButtonRight,_):: tail => Goto('End)

        case _ => Goto('End)
      }

    if(points.size > 0)
      shape = PolylineShape.fromPoints(points)
    }),
    'End -> ((events : List[Event]) => {

      Create(shape)

      //Clear the variables
      shape = PolylineShape.empty
      points = List[Vector2D]()
      previousPoint = None
    })
  )

  override def paint(g : Graphics, t : TransformationMatrix) {
    if (points.length > 0)
      g draw shape.transform(t)
  }
}