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

import java.awt.Color


/**
 * A module that measures and displays an area.
 */

//TODO: add ability to measure the area of one or more selected, closed polylines.
object Area extends Module {

  //a function to calculate an area defined by points.
  def area(points : List[Vector2D]) : Int = {

    var area : Double = 0
    var i : Int = 0

    //TODO: Add the first element to the end to close the polygon

    while (i < points.length - 1) {
      var pointX1 = points(i).x
      var pointY1 = points(i).y

      var pointX2 = points(i+1).x
      var pointY2 = points(i+1).y

      //area += (x.Items.Item(i) * y.Items.Item(i + 1) - x.Items.Item(i + 1) * y.Items.Item(i))
      area += pointX1 * pointY2 - pointX2 * pointY1
      i += 1
    }
    area = scala.math.abs(area * 0.5)
    area.toInt
  }

  lazy val eventHandler = new EventHandler(stateMap, stateMachine)

  var points = List[Vector2D]()

  lazy val anthracite  = new Color(0.25f, 0.25f, 0.25f, 0.30f)

  //TODO: Add a function to display cm2 or m2 instead of mm2 for large areas.

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

    //send a guide drawing the area dynamically as points are added:
    def getPointGuide = (p : Vector2D) => {

      if(!points.isEmpty) {
        val closedPolyline = points.reverse :+ p
        val areaGuide = closedPolyline.reverse :+ p
        Siigna.display("Area: "+area(areaGuide) +"mm2")
        PolylineShape(areaGuide).setAttributes("raster" -> anthracite, "StrokeWidth" -> 0.12)
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
      if (points.size > 2) {
        Siigna.display("Area: "+area(points) +"mm2")
      }

      //Clear the variables
      points = List[Vector2D]()
    })
  )
}