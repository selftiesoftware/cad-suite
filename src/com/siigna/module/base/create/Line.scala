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

/*package com.siigna.module.base.create

import com.siigna._
import app.controller.Controller
import app.Siigna

/**
 * A line module (draws one line-segment)
 */

object Line extends Module{

  var attributes : Attributes = Attributes()
  def set(name : String, attr : String) = Siigna.get(name).foreach((p : Any) => attributes = attributes + (attr -> p))

  private var points = List[Vector2D]()

  private var shape  : Option[LineShape] = None

  def stateMap = DirectedGraph(
    'StartCategory    ->   'Message  ->    'SetPoint
  )

  def stateMachine = Map(
    'StartCategory -> ((events : List[Event]) => {

      set("activeLineWeight", "StrokeWidth")
      set("activeColor", "Color")

      com.siigna.module.base.Default.previousModule = Some('Line)
      //Log.level += Log.DEBUG + Log.SUCCESS
      events match {
        case MouseDown(_, MouseButtonRight, _) :: tail => {
          Goto('End)
        }
        case _ => Module('Point)
      }
    }),
    'SetPoint -> ((events : List[Event]) => {

      def getPointGuide = (p : Vector2D) =>
        //TODO: find a better way to avoid empty list error when no point is set.
        if(points.size == 0) LineShape((p),(p))
        else {
          set("activeLineWeight", "StrokeWidth")
          set("activeColor", "Color")
          LineShape(points(0),p).setAttributes(attributes)
        }

      events match {
        // exit strategy
        case (MouseDown(_, MouseButtonRight, _) | MouseUp(_, MouseButtonRight, _) | KeyDown(Key.Esc, _)) :: tail => Goto('End, false)

        case Message(p : Vector2D) :: tail => {
          // save the point
          points = points :+ p
          // Define shape if there is enough points
          if (points.size == 1) {
            Module('Point)
            Controller ! Message(PointGuide(getPointGuide))
          } else if (points.size == 2) {
            shape = Some(LineShape(points(0),points(1)))
            Goto('End)
          }
        }

        // match on everything else
        case _ => {
          Module('Point)
          Controller ! Message(PointGuide(getPointGuide))
        }
      }
    }),
    'End -> ((events : List[Event]) => {
      if(shape.isDefined) CreateCategory(shape.get.setAttributes(attributes))

      //reset the module vars
      points = List[Vector2D]()
      shape = None

    })
  )
}*/