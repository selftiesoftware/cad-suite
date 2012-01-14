///* 2012 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

import com.siigna._

object Rectangle extends Module {

  def dynamicRectangleFromPoints(points : List[Vector2D], currentMouse : Vector2D) = {
    val p1 = Vector2D(points(1).x,points(1).y)
    val p2 = Vector2D(points(1).x,currentMouse.y)
    val p3 = Vector2D(currentMouse.x,currentMouse.y)
    val p4 = Vector2D(currentMouse.x,points(1).y)
    PolylineShape.fromPoints(p1,p2,p3,p4,p1)
  }

  val eventHandler = new EventHandler(stateMap, stateMachine)

  var points = List[Vector2D]()

  def rectangleFromPoints(points : List[Vector2D]) = {
    val p1 = Vector2D(points(1).x,points(1).y)
    val p2 = Vector2D(points(1).x,points(2).y)
    val p3 = Vector2D(points(2).x,points(2).y)
    val p4 = Vector2D(points(2).x,points(1).y)
    PolylineShape.fromPoints(p1,p2,p3,p4,p1)
  }

  var shape : PolylineShape = PolylineShape.empty

  def stateMap = DirectedGraph(
    'SecondPoint -> 'KeyEscape   -> 'End,
    'Start       -> 'KeyEscape   -> 'End
  )

  def stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      events match {
        case MouseUp(_, MouseButtonRight, _) :: tail => Goto('End)
        case MouseUp(point, _, _):: tail => {
          points = points :+ point
          println("initial points: "+point)
          if(points.length == 2) {
            println("points in start: "+points)
            Goto('SecondPoint)
          }
        }
        case _ =>
      }
    }),
    'SecondPoint -> ((events : List[Event]) => {
      events match {
        case MouseMove(position, _,_):: tail => {
            //draw a polyline from the points saved in shape
          println("in mouse move in second point")
          shape = dynamicRectangleFromPoints(points,position)
        }
        case MouseDown(point, _, _) :: tail => {
          points = points :+ point
          if(points.length == 3) {
            Goto('End)
          }
        }
        case _ =>
      }
      None
    }),
    'End -> ((events : List[Event]) => {
      println("in end")
      events match {
        case _ =>
        if (points.length >= 2) {
          shape = rectangleFromPoints(points)
          Create(shape)
        }
        else None
        //clear points
        points = List[Vector2D]()
      Default.previousModule = Some('Rectangle)
      }
    })
  )
  override def paint(g : Graphics, t : TransformationMatrix) {
    if (points.length > 0) {
      //println(currentMouse)
      g draw shape.transform(t)
    }
  }
}
