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

package com.siigna.module.base.create

import com.siigna._
import app.controller.Controller

object Rectangle extends Module {

  val eventHandler = new EventHandler(stateMap, stateMachine)

  var points = List[Vector2D]()

  def stateMap = DirectedGraph(
    'Start       -> 'Message   -> 'SetPoint
  )

  def stateMachine = Map(
    //TODO: draw a dummy rectangle of eg. 1/15 * 1/15 of the paper height/width dynamically before first point is set
    'Start -> ((events : List[Event]) => {
      events match {
        case MouseDown(_, MouseButtonRight, _) :: tail => Goto('End)
        case Message(p : Vector2D) :: tail => Goto('SetPoint)
        case _ => ForwardTo('Point, false)

      }
    }),
    'SetPoint -> ((events : List[Event]) => {
      //A function that passes a rectangleShape to the point module to be drawn dynamically
      //from the first point to the mouse position
      val getRectGuide : Vector2D => PolylineShape = (v : Vector2D) => {
        if(points.size > 0) {
          PolylineShape(Rectangle2D(points.head, v))
        }
        //TODO: a hack to prevent Error when calling Rectangle: Unexpected error in processing state map
        else PolylineShape(Rectangle2D(Vector2D(0,0), Vector2D(0,0)))
      }

      events match {
        case Message(point : Vector2D) :: tail => {
          if(points.length == 1) {
            points = points :+ point
            Goto('End)
          } else if (points.length == 0) {
           points = points :+ point
           Controller ! Message(PointGuide(getRectGuide))
           ForwardTo('Point)
          }
        }
        case _ =>
      }
    }),
    'End -> ((events : List[Event]) => {
      if (Siigna.double("activeLineWeight").isDefined && points.length == 2) {
        Create(PolylineShape(Rectangle2D(points(0), points(1))).setAttribute("StrokeWidth" -> Siigna.double("activeLineWeight").get))
      }
      else Create(PolylineShape(Rectangle2D(points(0), points(1))))

      // Clear variables
      points = List[Vector2D]()
      com.siigna.module.base.Default.previousModule = Some('Rectangle)
    })
  )

}