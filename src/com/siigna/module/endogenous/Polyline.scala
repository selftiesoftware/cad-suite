/* 2012 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

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
      println("events in PL start: "+events.head)
      events match {
        case MouseDown(_, MouseButtonRight, _) :: tail => {
          println("got RM in PL")
          Goto('End)
        }
        case _ => ForwardTo('Point)
      }
    }),
    'SetPoint -> ((events : List[Event]) => {
      def getPointGuide = {
        (p : Vector2D) => PolylineShape.fromPoints(points :+ p)
      }

      events match {
        case Message(p : Vector2D) :: tail => {
          // Save the point
          points = points :+ p

          // Define shape if there is enough points
          if (points.size > 1) {
            shape = Some(PolylineShape.fromPoints(points))
          }

          Send(Message(getPointGuide))
          ForwardTo('Point)
        }
        // Exit mechanisms
        case (MouseDown(_, MouseButtonRight, _) | KeyDown(Key.Esc, _)) :: tail => {
          Goto('End)
        }
        // Match on everything else
        case _ => {
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