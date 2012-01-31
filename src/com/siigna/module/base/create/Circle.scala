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

/* 2012 (C) Copyright by Siigna, all rights reserved. */

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
        }
        case MouseDown(p, _, _) :: tail => {
          center = Some(p)
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
          currentMouse = Some(p)
        }
        case MouseDown(r, _, _) :: tail => {
          radius = Some(r)
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