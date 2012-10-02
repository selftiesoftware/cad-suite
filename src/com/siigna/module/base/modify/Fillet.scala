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

/*package com.siigna.module.base.modify

import com.siigna.module.Module
import com.siigna._

/**
 *  A module to connect two line segments with an arc of a given radius (of just join them if the radius is zero)
 *
 *  1) Select two line segments A
 *  2) Check that they are not parallel, if so allow only 0 for radius (join lines)
 *  3) Prompt for radius (defaults to 0)
 *  4) Create guidelines B: offset lines r to the side where lines.angle < 180
 *  5) Get the guidelines B intersection (I), save as arc centerpoint
 *  6) Create guidelines C from I perpendicular to each line segment A
 *  7) Get intersection points between A and C (II), save as arc startPoint and endPoint.
 *  8) Create new line segments A using II as endpoints, and create arc.
 */

object Fillet extends Module{

  var line1 : Option[Shape] = None
  var line2 : Option[Shape] = None

  def stateMap = Map(
    'Start -> {
      if(!Drawing.selection.isEmpty) {
        Drawing.deselect()
        Siigna display "select first line"
      } else Siigna display "select first line"
      'SelectFirst
    },
    //select the first line segment for the fillet. Polyline segments should also be allowed.
    'SelectFirst -> {
      ForwardTo('Selection)
      if(Drawing.selection.isDefined) {
        val line = Drawing.selection.get.parts.head._1
        println(line)
        //save the shape
        line1 = Some(Drawing(line))
        println(line1.get)

        'SelectSecond
      }
    },
    //select the second line segment for the fillet. Polyline segments should also be allowed.
    'SelectSecond -> {
      ForwardTo('Selection)
      if(Drawing.selection.isDefined) {
        val line = Drawing.selection.get.parts.head._1
        println(line)
        //save the shape
        line2 = Some(Drawing(line))
        println(line2.get)

        'SelectSecond
      }
    },
    //prompt for a radius
    'Radius -> ((events : List[Event]) => {
       println("in radius")
    }),
    //join the lines with an arc of the given radius. This requires the endpoints to be moved to where the lines and arc tangent meet.
    'End -> ((events : List[Event]) => {
      println("ending fillet")
    })
  )
}*/