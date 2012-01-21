///* 2012 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

import com.siigna._

object Rectangle extends Module {

  def rectangleFromPoints(point1 : Vector2D, point2 : Vector2D) : PolylineShape = {
    val p1 = Vector2D(point1.x,point1.y)
    val p2 = Vector2D(point1.x,point2.y)
    val p3 = Vector2D(point2.x,point2.y)
    val p4 = Vector2D(point2.x,point1.y)
    PolylineShape.fromPoints(p1,p2,p3,p4,p1)
  }
  //a function that is used by the point module to draw the rectangle dynamically
  def pointGuide(point1 : Vector2D, point2 : Vector2D) = {
      val p1 = Vector2D(point1.x,point1.y)
      val p2 = Vector2D(point1.x,point2.y)
      val p3 = Vector2D(point2.x,point2.y)
      val p4 = Vector2D(point2.x,point1.y)
      PolylineShape.fromPoints(p1,p2,p3,p4,p1)
  }

  val eventHandler = new EventHandler(stateMap, stateMachine)

  var points = List[Vector2D]()

  var shape : PolylineShape = PolylineShape.empty

  def stateMap = DirectedGraph(

    'Start       -> 'KeyEscape   -> 'End,
    //'SecondPoint -> 'KeyDown     -> 'Point,
    'SecondPoint -> 'KeyEscape   -> 'End
  )

  def stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      events match {
        //if the point module returns a valid point, use this as the first corner of the rectangle.
        case Message(point : Vector2D) :: tail => {
          points = points :+ point
          Goto('SecondPoint)
        }
        //if there is no point, go to the point module
        case _ => ForwardTo('Point)
      }
    }),
    'SecondPoint -> ((events : List[Event]) => {
      //clear any messages that might be visible
      interface.clearDisplay()

      events match {
        case MouseMove(position, _, _):: tail => {
          //make a function here that passes a rectangleShape to the point module to be drawn dynamically
          //from the first point to the mouse position
          Send(Message(pointGuide(_ ,_)))
          ForwardTo('Point)
         //if(points.length == 1)
         //   shape = rectangleFromPoints(points(0),position)
        }
        case Message(point : Vector2D) :: tail => {
          if(points.length == 2) {
            points = points :+ point
            shape = rectangleFromPoints(points(0),points(1))
            Goto('End)
          }
        }
        case MouseUp(_, _, _) :: tail =>
        case _ =>
      }
      None
    }),
    'End -> ((events : List[Event]) => {
      if (points.length == 3)
        Create(shape)

      points = List[Vector2D]()
      shape = PolylineShape.empty
      //tell the default module that last module was rect, so that it can be recalled with SPACE
      Default.previousModule = Some('Rectangle)
    })
  )
  override def paint(g : Graphics, t : TransformationMatrix) {
    if (points.length > 0) {
      g draw shape.transform(t)
    }
  }
}