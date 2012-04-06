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

object Polyline extends Module {

  // The points of the polyline
  private var points   = List[Vector2D]()

  // The polylineshape so far
  private var shape : Option[PolylineShape] = None

  val eventHandler = EventHandler(stateMap, stateMachine)

  def stateMap = DirectedGraph(
    'Start    ->   'Message  ->    'SetPoint
  )

  def stateMachine = Map(
  'Start -> ((events : List[Event]) => {
    //Log.level += Log.DEBUG + Log.SUCCESS
    events match {
      case MouseDown(_, MouseButtonRight, _) :: tail => {
        Goto('End)
      }
      case _ => ForwardTo('Point, false)
    }
  }),
  'SetPoint -> ((events : List[Event]) => {
    def getPointGuide = (p : Vector2D) => PolylineShape.fromPoints(points :+ p)
    events match {
      // Exit strategy
      case (MouseDown(_, MouseButtonRight, _) | MouseUp(_, MouseButtonRight, _) | KeyDown(Key.Esc, _)) :: tail => Goto('End, false)

      case Message(p : Vector2D) :: tail => {
        // Save the point
        points = points :+ p
        // Define shape if there is enough points
        if (points.size > 1) {
          shape = Some(PolylineShape.fromPoints(points))
        }
        ForwardTo('Point, false)
        Send(Message(PointGuide(getPointGuide)))
      }

      // Match on everything else
      case _ => {
        ForwardTo('Point)
        Send(Message(PointGuide(getPointGuide)))
      }
    }
  }),
  'End -> ((events : List[Event]) => {
    // If the shape is defined, then create it!
    if (shape.isDefined)
      Create(shape.get)

    //clear the vars
    shape = None
    points = List()
  }))
}