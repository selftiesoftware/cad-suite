package com.siigna.module.base.create

///* 2012 (C) Copyright by Siigna, all rights reserved. */

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
      println(events.head)
      events match {
        case MouseDown(_, MouseButtonRight, _) :: tail => Goto('End)
        case Message(p : Vector2D) :: tail => Goto('SetPoint)
        case _ => ForwardTo('Point)

      }
    }),
    'SetPoint -> ((events : List[Event]) => {
      //A function that passes a rectangleShape to the point module to be drawn dynamically
      //from the first point to the mouse position
      val getRectGuide : Vector2D => PolylineShape = (v : Vector2D) => {
        PolylineShape.fromRectangle(Rectangle2D(points.head, v))
      }
      events match {
        case Message(point : Vector2D) :: tail => {
          println("back from Point" +points)
          if(points.length == 1) {

            points = points :+ point
            Goto('End)
          }
          else if(points.length == 0) {
           points = points :+ point
           Send(Message(PointGuide(getRectGuide)))
           ForwardTo('Point)
          }
        }
        case _ => {

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