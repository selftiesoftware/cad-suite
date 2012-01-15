/* 2012 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

import com.siigna._

object Polyline extends Module {

  var angleGuide : Double = 0
  val eventHandler = EventHandler(stateMap, stateMachine)
  var points   = List[Vector2D]()
  var shape : PolylineShape = PolylineShape.empty
  var message : Option[Double] = None

  def stateMap = DirectedGraph(
    'Start        -> 'KeyEscape  -> 'End
    //'Start        -> 'KeyDown    -> 'End
  )

  def stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      //TODO: when returning from Angle Gizmo, a message should be included in the event stream, but it is not??
      //println("events::" + events)
      events match {
        case Message(p : Option[Double]) :: tail => {
          angleGuide = p.get
          message = Some(angleGuide)
          //activate the event parser that corrects angles if the angle gizmo is used
          //eventParser.snapTo(new RadianSnap(Siigna.mousePosition, angleGuide))
        }
        case MouseUp(_, MouseButtonRight, _) :: tail => Goto('End)
        case MouseDown(point, MouseButtonLeft, _):: tail => {
          // if the first point is defined, and the module is not using the angle gizmo to define a radian, set the next point
          println("has angle msg?: "+message)
          if (!message.isDefined) {
            points = points :+ point
            //draw a polyline from the points saved in shape
            shape = PolylineShape.fromPoints(points)
          }
          // else use the Radian parser to define the next point on a given radian, based on Angle Gizmo return:
          else {
            points = points :+ point
            //points = points :+ RadianSnap(point,AngleGizmo.message.get)
            println("DRAW PARSED POINT HERE")
          }
          Preload('AngleGizmo, "com.siigna.module.endogenous.AngleGizmo")
          //sends a point that the angleGizmo will use if the angle Gizmo needs to be drawn
          AngleGizmo.receivedPoint = Some(point)
          ForwardTo('AngleGizmo)


        }
        case MouseUp(_, MouseButtonRight, _):: tail => Goto ('End)
        case KeyDown(Key.Enter, _) :: tail => Goto ('End)
        case KeyUp(Key.Space, _) :: tail => Goto ('End)
        case MouseMove(position, _, _):: tail => {
          //if there is a message, parse the line
          println("MM PL")
          println("message: "+message)
          if(message.isDefined) {
            println("MM, PL, and message")
            eventParser.snapTo(new RadianSnap(Siigna.mousePosition, message.get))
          }
        }
        case _ =>
      }
    }),
    'End -> ((events : List[Event]) => (
      events match {
        case _ =>
          //create the final polyline
          Create(shape)

          //clear the points list
          points = List[Vector2D]()
          angleGuide = 0
          message = None
      Default.previousModule = Some('Polyline)
      })
    )
  )
  override def paint(g : Graphics, t : TransformationMatrix) {
    if (points.length > 0) {
      //draw the polyline
      g draw shape.transform(t)
      //draw the next segment, unless the angle Gizmo is active
      //if (!AngleGizmo.inAngleGizmoMode == true) {
        //draw a the current mouse position, transformed by the active radian if the angle gizmo is active
        g draw LineShape(Siigna.mousePosition,points.last).transform(t)
      //}
    }
  }
}