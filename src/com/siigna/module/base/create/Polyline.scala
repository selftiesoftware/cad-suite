package com.siigna.module.base.create

/* 2012 (C) Copyright by Siigna, all rights reserved. */

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
      // Exit mechanisms
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
        ForwardTo('Point, false)
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