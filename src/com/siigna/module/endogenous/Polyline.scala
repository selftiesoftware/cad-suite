/* 2012 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

import com.siigna._

object Polyline extends Module {

  var angleGuide : Double = 0
  val eventHandler = EventHandler(stateMap, stateMachine)
  var points   = List[Vector2D]()
  var currentMouse = Vector2D(0,0)
  var shape : PolylineShape = PolylineShape.empty
  var message : Option[Double] = None



  def stateMap = DirectedGraph(
    'Start        -> 'KeyEscape  -> 'End
    //'Start        -> 'KeyDown    -> 'End
  )

  def stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      events match {
        case MouseUp(_, MouseButtonRight, _) :: tail => Goto('End)
        case MouseDown(point, MouseButtonLeft, _):: tail => {
          points = points :+ point
          //if (message.isDefined){
          if (points.size > 0) {
            //draw a polyline from the points saved in shape
            shape = PolylineShape.fromPoints(points)
            Preload('AngleGizmo, "com.siigna.module.endogenous.AngleGizmo")
            //sends a point that the angleGizmo will use if the angle Gizmo needs to be drawn
            AngleGizmo.receivedPoint = Some(point)
            ForwardTo('AngleGizmo)

          }
        }
        case Message(p : Double) :: tail => {
          angleGuide = p
          println(angleGuide)
        }
        case MouseUp(_, MouseButtonRight, _):: tail => Goto ('End)
        //TODO: enable the use of ENTER to finish a polyline. Currently this will cause an error.
        case KeyDown(Key.Enter, _) :: tail => Goto ('End)
        case KeyUp(Key.Space, _) :: tail => Goto ('End)
        case MouseMove(position, _, _):: tail => {
          //store the current mouse position in a var
          currentMouse = position
          //parse the events with the gizmo angle
          eventParser.snapTo(new RadianSnap(currentMouse, angleGuide))
          None
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
          message = None
      Default.previousModule = Some('Polyline)
      })
    )
  )

  override def paint(g : Graphics, t : TransformationMatrix) {
    if (points.length > 0) {
      g draw shape.transform(t)
      //draw a the current mouse position, transformed by the active radian if the angle gizmo is active
      g draw LineShape(currentMouse,points.last).transform(t)
    }
  }
}