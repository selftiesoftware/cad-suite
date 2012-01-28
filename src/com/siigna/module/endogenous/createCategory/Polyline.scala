package com.siigna.module.endogenous.createCategory

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
    events match {
        case MouseDown(_, MouseButtonRight, _) :: tail => {
          Goto('End)
        }
        case _ => ForwardTo('Point, false)
      }
    }),
  'SetPoint -> ((events : List[Event]) => {
    def getPointGuide = {
        (p : Vector2D) => PolylineShape.fromPoints(points :+ p)
      }
      events match {
        case Message(p : Vector2D) :: tail => {
          // Save the point
          println("added point to polyline: "+p)
          points = points :+ p
          // Define shape if there is enough points
          if (points.size > 1) {
            shape = Some(PolylineShape.fromPoints(points))
          }

          ForwardTo('Point, false)
          Send(Message(getPointGuide))
        }
        // Exit mechanisms
        case (MouseDown(_, MouseButtonRight, _) | MouseUp(_, MouseButtonRight, _) | KeyDown(Key.Esc, _)) :: tail => {
          Goto('End)
        }
        // Match on everything else
        case _ => {
          println("sending point guide from PL")
          Send(Message(PointGuide(getPointGuide)))
          ForwardTo('Point)
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
    })
  )

  override def paint(g : Graphics, t : TransformationMatrix) {
    if (shape.isDefined)
      g draw shape.get.transform(t)
  }
}