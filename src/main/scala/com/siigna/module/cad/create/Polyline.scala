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
  private var firstPoint: Option[Vector2D] = None
  private var firstPointSet : Boolean = false

  private val vector2DGuide = Vector2DGuide((v : Vector2D) => {
    Traversable(PolylineShape(points :+ v).addAttributes(attributes))
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
      //Exit strategy - right click finishes polyline, if it has 2 or more points.
      case (End ) :: tail => End
      case (MouseDown(_, MouseButtonRight, _) | End(MouseDown(_,MouseButtonRight, _))
            |KeyDown(Key.Esc, _) | End(KeyDown(Key.escape, _))
            |KeyDown(Key.space, _) | End(KeyDown(Key.space, _))) :: tail => {
        finalisePolyline
        End
      }

      case End(MouseDown(v : Vector2D, _, _)) :: tail => {
        firstPoint = Some(v)
        points = points :+ v
        Start('cad, "create.Input", InputRequest(7,None,Vector2DGuide((v : Vector2D) => Traversable(LineShape(firstPoint.get, v).addAttributes(attributes)))))
      }
      //Handle values returned from input
      case End(v : Vector2D) :: tail => {
        if (firstPoint.isEmpty) {
          firstPoint = Some(v)
          points = points :+ v
          Start('cad, "create.Input", InputRequest(8,None,Vector2DGuide((v : Vector2D) => Traversable(LineShape(firstPoint.get, v).addAttributes(attributes)))))
        } else if (firstPointSet == false) {
          firstPointSet = true
          if (v != firstPoint.get & v.distanceTo(firstPoint.get) > Siigna.selectionDistance) {
            points = points :+ v
          }
          Start('cad,"create.Input", InputRequest(7,Some(points.last),vector2DGuide))
        } else {
          points = points :+ v
          Start('cad,"create.Input", InputRequest(7,Some(points.last),vector2DGuide))
        }
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
        else End
      }
      case x => {
        if (points.length == 0) {
          //change cursor to crosshair
          Siigna.setCursor(Cursors.crosshair)
          //Update tooltip
          Tooltip.updateTooltip(List("Polyline tool active. Right click to finish polyline."))
          //Request input
          Start('cad, "create.Input", InputRequest(20,None))
        } else {
        println("Polyline module can't interpret this: " + x)
        if (points.length > 0 ) Start('cad,"create.Input", InputRequest(7,Some(points.last),vector2DGuide))
        else Start('cad, "create.Input", InputRequest(20,None))
        }
      }
    }
  )
}