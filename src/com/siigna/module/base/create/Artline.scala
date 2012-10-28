/*
 * Copyright (c) 2012. Siigna is released under the creative common license by-nc-sa. You are free
 * to Share — to copy, distribute and transmit the work,
 * to Remix — to adapt the work
 *
 * Under the following conditions:
 * Attribution —  You must attribute the work to http://siigna.com in the manner specified by the author or licensor (but not in any way that suggests that they endorse you or your use of the work).
 * Noncommercial — You may not use this work for commercial purposes.
 * Share Alike — If you alter, transform, or build upon this work, you may distribute the resulting work only under the same or similar license to this one.
 */

/*package com.siigna.module.base.create

import com.siigna._

/**
 * A freehand Artline pen
 * NOTES: how to optimise and expand this module??
 *
 * 1) Ink-pen
 * 2) this pencil
 * 3) a new 'digital aesthetics' to freehand drawing? ideas:
 * pixels, the pointer creates a 'tail of colored pixels'
 * fixed size line segments
 * linework made of polylines and arcs
 *
 */

class Artline extends Module {

    var currentMouse = Vector2D(0,0)

    //val buffer = new scala.collection.mutable.ListBuffer[Vector2D]
    val eventHandler = EventHandler(stateMap, stateMachine)
    var dotshape = Vector2D(10,10)
    var points = List[Vector2D]()
    var startPoint : Option[Vector2D] = None
    var currentPoint : Option[Vector2D] = None
    var shape : PolylineShape = PolylineShape.empty

    def hasPoint = (points.size >= 1)

    def stateMap = DirectedGraph(
      'StartCategory         -> 'KeyEscape ->         'End,
      'StartCategory         -> 'MouseMove   ->       'Points
    )

    def stateMachine = Map(
      'StartCategory -> ((events : List[Event]) => {
        events match {
          case MouseUp(_, _, _) :: tail => {
          }
          case _ =>
        }
      None
      }),
      //draw single points on mouseclick
      'Points -> ((events : List[Event]) => {
        events match {
            case MouseDown(p, _, _) :: tail => {
              startPoint = Some(p)
              //points = List(LineShape(dotshape, point),LineShape(dotshape, point))
              //Message(points)
            }
            case MouseDrag(p, _, _) :: tail => {
              currentPoint = Some(p)
              if (startPoint.get.distanceTo(currentPoint.get) > 5) {
                points = points :+ startPoint.get
                startPoint = currentPoint
               //draw a polyline from the points saved in shape
               shape =  PolylineShape(points)
              }
            }
            case MouseUp(_, MouseButtonRight, _) :: tail => Goto('End)
            case MouseUp(p, _, _) :: tail => {
              points = points :+p
              shape = PolylineShape(points)
              CreateCategory(shape.asInstanceOf[Shape])
              //clear the list
              shape = PolylineShape.empty
              points = List[Vector2D]()
              Goto('StartCategory)
            }
            case _ => None
         }
      None
      }),

      'End -> ((events : List[Event]) => {
        events match {
          case KeyDown(Key.Escape, _) :: tail => None
          case _ => {
            if (points.size > 0){
            //draw a polyline from the points saved in the path
            //shape = PolylineShape.fromPoints(points)
            //CreateCategory(shape)
            }
          }
        com.siigna.module.base.Default.previousModule = Some('Artline)
        }
        None
      })
    )
    override def paint(g : Graphics, t : TransformationMatrix) {
      if (points.length > 0 && currentPoint.isDefined) {
        g draw shape.transform(t)
        g draw LineShape(currentPoint.get,points.last).transform(t)
      }

    }
}*/