/* 2012 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

import com.siigna._

object Circle extends Module {

  val eventHandler = EventHandler(stateMap, stateMachine)

  var center : Option[Vector2D] = None
  var radius : Option[Vector2D] = None
  var currentMouse  : Option[Vector2D] = None

  def stateMap = DirectedGraph(
    //'Start        -> 'SetRadius  -> 'End,
    //'Start        -> 'MouseUp  -> 'SetRadius,
    'Start        -> 'KeyEscape  -> 'End,
    'Start        -> 'KeyDown    -> 'End
  )

  def stateMachine = Map(
    //Start: Defines a centerpoint for the circle and forwards to 'SetRadius
    'Start -> ((events : List[Event]) => {
      events match {
        case MouseUp(_, MouseButtonRight, _) :: tail => Goto('End)
        case MouseMove(_, _, _) :: tail => {
          println("moving in set Center")
        }
        case MouseDown(p, _, _) :: tail => {
          center = Some(p)
          println("center set at: "+center)
        }
         case MouseUp(_, _, _) :: tail => {
           if (center.isDefined) {
           Goto('SetRadius)
           }
         }
        case _ =>
      }
      None
    }),

    //SetRadius: Listens for the radius of the circle and forwards to 'End
    'SetRadius -> ((events : List[Event]) => {
      events match {
        case MouseMove(p, _, _) :: tail => {
          println("moving in setRadius")
          currentMouse = Some(p)
        }
        case MouseDown(r, _, _) :: tail => {
          radius = Some(r)
          println("Radius set at:" +radius)
        }
        case MouseUp(_, _, _) :: tail => {
           if (radius.isDefined) {
           Goto('End)
           }
         }

        //case MouseUp(_, _, _) :: tail => Goto('End)
        case _ => None
      }
    }),

    'End -> ((events : List[Event]) => (
      events match {
        case _ =>
          //create the circle
          println("center: "+center)
          println("final radius: "+currentMouse)
          Create(CircleShape(center.get,radius.get))

          //clear the points list
          center = None
          radius = None
      })
    )
  )
override def paint(g : Graphics, t : TransformationMatrix) {

      g draw CircleShape(center.get, currentMouse.get).transform(t)

  }
}