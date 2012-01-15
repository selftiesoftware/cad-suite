/* 2012 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

import com.siigna._

object Polyline extends Module {

  //var angleGuide : Message[Double] = None
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
        //case Message(p : Vector2D) :: tail => {
        //  println(p)
        //}
        //case Message(p: Double):: tail => {
        //  message = Some(p)
        //  println("events:"+events)
        //  println("message: "+p)
        //}
        case MouseDown(point, MouseButtonLeft, _):: tail => {
          points = points :+ point
          //if (message.isDefined){
          if (points.size > 0) {
            //draw a polyline from the points saved in shape
            shape = PolylineShape.fromPoints(points)
            Preload('AngleGizmo, "com.siigna.module.endogenous.AngleGizmo")
            ForwardTo('AngleGizmo)
          }
        }
        case Message(p) :: tail => {
          println("message, "+p)
        }
        case MouseUp(_, MouseButtonRight, _):: tail => Goto ('End)
        //TODO: enable the use of ENTER to finish a polyline. Currently this will cause an error.
        //case KeyDown(Key.Enter, _) :: tail => Goto ('End)
        case KeyUp(Key.Space, _) :: tail => Goto ('End)
        case MouseMove(position, _, _):: tail => {
          //store the current mouse position in a var
          currentMouse = position
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
      g draw LineShape(currentMouse,points.last).transform(t)
    }
  }
}