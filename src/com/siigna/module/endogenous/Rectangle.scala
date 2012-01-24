///* 2012 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

import com.siigna._

object Rectangle extends Module {

  val eventHandler = new EventHandler(stateMap, stateMachine)

  var points = List[Vector2D]()

  var shape : PolylineShape = PolylineShape.empty

  def stateMap = DirectedGraph(
    'Start       -> 'KeyEscape   -> 'End,
    //'SecondPoint -> 'KeyDown     -> 'Point,
    'SecondPoint -> 'KeyEscape   -> 'End
  )

  def stateMachine = Map(
    //TODO: draw a dummy rectangle of eg. 1/15 * 1/15 of the paper height/width dynamically before first point is set
    'Start -> ((events : List[Event]) => {
      events match {
        //if the point module returns a valid point, use this as the first corner of the rectangle.
        case Message(point : Vector2D) :: tail => {
          points = points :+ point
          Goto('SecondPoint, false)
        }
        case _ => {
          // Forward to point
          ForwardTo('Point)
        }
      }
    }),
    'SecondPoint -> ((events : List[Event]) => {
      events match {
        case Message(point : Vector2D) :: tail => {
          points = points :+ point
          Goto('End)
        }
        case _ => {
          //Make a function here that passes a rectangleShape to the point module to be drawn dynamically
          //from the first point to the mouse position
          val guide : Vector2D => PolylineShape = (v : Vector2D) => {
            PolylineShape.fromRectangle(Rectangle2D(points.head, v))
          }

          // Send on the message
          ForwardTo('Point)
          Send(Message(PointGuide(guide)))
        }
      }
    }),
    'End -> ((events : List[Event]) => {
      if (points.length == 2) {
        Create(PolylineShape.fromRectangle(Rectangle2D(points(0), points(1))))
      }

      // Clear variables
      points = List[Vector2D]()
      shape = PolylineShape.empty
    })
  )

}