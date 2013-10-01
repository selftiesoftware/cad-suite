/*
 * Copyright (c) 2008-2013, Selftie Software. Siigna is released under the
 * creative common license by-nc-sa. You are free
 *   to Share — to copy, distribute and transmit the work,
 *   to Remix — to adapt the work
 *
 * Under the following conditions:
 *   Attribution —   You must attribute the work to http://siigna.com in
 *                    the manner specified by the author or licensor (but
 *                    not in any way that suggests that they endorse you
 *                    or your use of the work).
 *   Noncommercial — You may not use this work for commercial purposes.
 *   Share Alike   — If you alter, transform, or build upon this work, you
 *                    may distribute the resulting work only under the
 *                    same or similar license to this one.
 *
 * Read more at http://siigna.com and https://github.com/siigna/main
 */

package com.siigna.module.cad.create

import com.siigna._
import app.Siigna
import module.Tooltip
import java.awt.Cursor

class Polyline extends Module {

  private val attributes = {
    val color = Siigna.color("activeColor")
    val lineWidth = Siigna.double("activeLineWidth")
    Attributes(Seq(color.map(c => "Color" -> color.getOrElse(None)), lineWidth.map(w => "StrokeWidth" -> lineWidth.getOrElse(None))).flatten)
  }

  private var points   = List[Vector2D]()

  private val vector2DGuide = Vector2DGuide((v : Vector2D) => {
    Iterable(PolylineShape(points :+ v).addAttributes(attributes))
  })

  private def finalisePolyline: Boolean = {
    if (points.length > 1) {
      val polyline = PolylineShape(points).addAttributes(attributes)
      Create(polyline)
      true
    } else false
  }

  val stateMap: StateMap = Map(
    'Start -> {
      case _ => {
        //change cursor to crosshair
        Siigna.setCursor(Cursors.crosshair)
        //Update tooltip
        Tooltip.updateTooltip("Polyline tool active. Right click to finish polyline.")
        //Request input
        Start('cad, "create.Input", InputRequest(6,None))
        //Goto next shape
        'ReceivePoints
      }
    },

    'ReceivePoints -> {
      //Exit strategy - right click finishes polyline, if it has 2 or more points.
      case (KeyDown(Key.Esc, _) | End(KeyDown(Key.escape, _))) :: tail => End
      case (MouseDown(_, MouseButtonRight, _) | End(MouseDown(_,MouseButtonRight, _))) :: tail => {
        if (finalisePolyline) End
        else Start('cad,"create.Input", InputRequest(7,Some(points.last),vector2DGuide))
      }

      //Handle values returned from input
      case End(v : Vector2D) :: tail => {
        points = points :+ v
        Start('cad,"create.Input", InputRequest(7,Some(points.last),vector2DGuide))
      }

      //Handle enter, backspace and unexpected events
      //Enter ends polyline, if there is at least 2 points
      case End(KeyDown(Key.enter,modifier)) :: tail => {
        if (finalisePolyline) End
        else Start('cad,"create.Input", InputRequest(7,Some(points.last),vector2DGuide))
      }
      //Backspace removes last point
      case End(KeyDown(Key.backspace,modifier)) :: tail => {
        if (points.length > 0) {
          points = points.dropRight(1)
        }
        if (points.length > 0 ) Start('cad,"create.Input", InputRequest(7,Some(points.last),vector2DGuide))
        else Start('cad, "create.Input", InputRequest(6,None))
      }
      case x => {
        println("Polyline module can't interpret this: " + x)
        if (points.length > 0 ) Start('cad,"create.Input", InputRequest(7,Some(points.last),vector2DGuide))
        else Start('cad, "create.Input", InputRequest(6,None))
      }
    }
  )
}