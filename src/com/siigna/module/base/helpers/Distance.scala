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

package com.siigna.module.base.helpers

import com.siigna._
import com.siigna.module.Module
import app.controller.Controller
import com.siigna.module.base.create._

/**
 * A module that measures and displays a distance between two points
 */

object Distance extends Module {

  lazy val eventHandler = new EventHandler(stateMap, stateMachine)

  var points = List[Vector2D]()


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

      def getPointGuide = (p : Vector2D) =>

        if(points.size == 0) LineShape((p),(p))
        else {
          Siigna.display("distance:  " + (p-points(0)).length.round + " millimeters")
          LineShape(points(0),p)
        }

      events match {
        // exit strategy
        case (MouseDown(_, MouseButtonRight, _) | MouseUp(_, MouseButtonRight, _) | KeyDown(Key.Esc, _)) :: tail => Goto('End, false)

        case Message(p : Vector2D) :: tail => {
          // save the point
          points = points :+ p
          // Define shape if there is enough points
          if (points.size == 1) {
            ForwardTo('Point, false)
            Controller ! Message(PointGuide(getPointGuide))
          } else Goto('End)
        }

        // match on everything else
        case _ => {
          ForwardTo('Point)
          Controller ! Message(PointGuide(getPointGuide))
        }
      }
    }),

    'End -> ((events : List[Event]) => {
      if (points.size == 2) {
        Siigna.display("distance:  " + ((points(1)-points(0)).length.round) + " millimeters")
      }

      //clear the points list
      points = List()
    })
  )
}
