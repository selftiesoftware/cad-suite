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
import app.Siigna
import com.siigna.module.Module
import app.controller.Controller

import java.awt.Color

object Fill extends Module {

  var points = List[Vector2D]()

  lazy val anthracite  = new Color(0.25f, 0.25f, 0.25f, 0.30f)

  def stateMap = DirectedGraph(

    'Start    ->   'Message  ->    'SetPoint,
    'Start    ->   'KeyDown  ->    'End
  )

  def stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      events match {
        case MouseDown(_, MouseButtonRight, _) :: tail => {
          'End
        }
        case _ => ForwardTo('Point, false)
      }
    }),
  'SetPoint -> ((events : List[Event]) => {

    //send a guide drawing the area dynamically as points are added:
    def getPointGuide = (p : Vector2D) => {

      if(!points.isEmpty) {
        val closedPolyline = points.reverse :+ p
        val areaGuide = closedPolyline.reverse :+ p
          if(Siigna.get("activeColor").isDefined)) PolylineShape((areaGuide)).setAttributes("raster" -> Siigna("activeColor"), "StrokeWidth" -> 0.0)
          else PolylineShape((areaGuide)).setAttributes("raster" -> anthracite, "StrokeWidth" -> 0.0)
    }
      else PolylineShape(Vector2D(0,0),Vector2D(0,0))
    }
    events match {
      // Exit strategy
      case (MouseDown(_, MouseButtonRight, _) | MouseUp(_, MouseButtonRight, _) | KeyDown(Key.Esc, _)) :: tail => Goto('End, false)

      case Message(p : Vector2D) :: tail => {
        // Save the point
        points = points :+ p
        // Define shape if there is enough points
        ForwardTo('Point, false)
        Controller ! Message(PointGuide(getPointGuide))
      }

      // Match on everything else
      case _ => {
        ForwardTo('Point)
        Controller ! Message(PointGuide(getPointGuide))
      }
    }
  }),
    'End -> ((events : List[Event]) => {
      //add a line segment from the last to the first point in the list to close the fill area
      if (points.size > 2 && Siigna.get("activeColor").isDefined)
        Create(PolylineShape(points :+ points(0)).setAttributes("raster" -> Siigna("activeColor"), "StrokeWidth" -> 0.0))
      else Create(PolylineShape(points :+ points(0)).setAttributes("raster" -> anthracite, "StrokeWidth" -> 0.0))

      //Clear the variables
      points = List[Vector2D]()
      com.siigna.module.base.Default.previousModule = Some('Area)

    })
  )
}*/