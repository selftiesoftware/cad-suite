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
import app.controller.Controller

object Rectangle extends Module {

  var attributes : Attributes = Attributes()

  var points = List[Vector2D]()

  def set(name : String, attr : String) = Siigna.get(name).foreach((p : Any) => attributes = attributes + (attr -> p))

  def stateMape = Map(
    //TODO: draw a dummy rectangle of eg. 1/15 * 1/15 of the paper height/width dynamically before first point is set
    'Start -> {
      set("activeLineWeight", "StrokeWidth")
      set("activeColor", "Color")

      events match {
        case MouseDown(_, MouseButtonRight, _) :: tail => 'End
        case Message(p : Vector2D) :: tail => 'SetPoint
        case _ => ForwardTo('Point, false)

      }
    },
    'SetPoint -> {
      case events => {
        //A function that passes a rectangleShape to the point module to be drawn dynamically
        //from the first point to the mouse position
        val getRectGuide : Vector2D => PolylineShape = (v : Vector2D) => {
          if(points.size > 0) {
            PolylineShape(Rectangle2D(points.head, v)).setAttributes(attributes)
          }
          //TODO: a hack to prevent Error when calling Rectangle: Unexpected error in processing state map
          else PolylineShape(Rectangle2D(Vector2D(0,0), Vector2D(0,0)))
        }

        events match {
          case Message(point : Vector2D) :: tail => {
            if(points.length == 1) {
              points = points :+ point
              'End
            } else if (points.length == 0) {
              points = points :+ point
              //Controller ! Message(PointGuide(getRectGuide))
              ForwardTo('Point)
            }
          }
          case _ =>
        }
      }
    },
    'End -> {
      if(!points.isEmpty) Create(PolylineShape(Rectangle2D(points(0), points(1))).setAttributes(attributes))

      // Clear variables
      points = List[Vector2D]()
      com.siigna.module.base.Default.previousModule = Some('Rectangle)
    }
  )

}