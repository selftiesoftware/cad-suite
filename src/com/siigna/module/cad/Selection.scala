/*
 * Copyright (c) 2008-2013. Siigna is released under the creative common license by-nc-sa. You are free
 * to Share — to copy, distribute and transmit the work,
 * to Remix — to adapt the work
 *
 * Under the following conditions:
 * Attribution —  You must attribute the work to http://siigna.com in the manner specified by the author or licensor (but not in any way that suggests that they endorse you or your use of the work).
 * Noncommercial — You may not use this work for commercial purposes.
 * Share Alike — If you alter, transform, or build upon this work, you may distribute the resulting work only under the same or similar license to this one.
 */

package com.siigna.module.cad

import com.siigna._
import java.awt.Color

/**
 * A Module for selecting shapes.
 */
class Selection extends Module {

  private var box : Option[SimpleRectangle2D] = None

  var nearestShape : Option[(Int, Shape)] = None

  // The starting point of the rectangle
  private var startPoint : Option[Vector2D] = None

  /**
   * Examines whether the selection is currently enclosed (selects the entire shapes) or not (only selects parts).
   */
  def isEnclosed : Boolean = startPoint.isDefined && startPoint.get.x <= mousePosition.x

  def stateMap = Map(
  'Start -> {

      //case events => println(events)
      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End

      //if SHIFT is pressed the selection module does not end, but listens after events.
      //in case of a mouse down, a de-selection is made.

      case MouseUp(p, _, _) :: tail => {
        val m = mousePosition.transform(View.deviceTransformation)
        Select(Drawing(m), m)
        End
      }

      //if ModuleInit forwards to selection with a mouse drag
      // 1: If a shape-part is hit, it is selected (so it will be moved)
      // 2: If not a shape-part is hit, a drag box is initiated:
      case (m : MouseDrag) :: tail => {
        startPoint = Some(m.position)
        'Box
      }

      //double click anywhere on a shape selects the full shape.
      case Start(_,MouseDouble(p,_,_)) :: tail => {
        val m = mousePosition.transform(View.deviceTransformation)
        Select(Drawing(m), m)
        End
      }

      case MouseDrag(p, button, modifier) :: tail =>  //SHIFT USED (not input from ModuleInit)
      case MouseMove(_,_,_) :: tail =>
      case f => { println("Selection recieved an unknown input: " + f)}
    },

    'Box -> {
      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      case MouseDown(p, MouseButtonRight, _) :: tail => End

      case MouseDrag(p, _, _) :: tail => {
        box = Some(Rectangle2D(startPoint.get, p))
      }
      case MouseUp(p, _, _) :: tail => {
        Select(box.get.transform(View.deviceTransformation), isEnclosed)
        End
      }
      case e => End
    }
  )

  private val enclosed = "#8899CC".color
  private val focused  = "#CC8899".color
  private val rasterEnclosed = new Color(100, 120, 210, 30)
  private val rasterFocused  = new Color(210, 100, 120, 30)

  override def paint(g : Graphics, t : TransformationMatrix) {

    if (box.isDefined) {
      val p = PolylineShape(box.get).setAttribute("Color" -> (if (isEnclosed) enclosed else focused))
      val r = p.setAttributes("Raster" -> (if (isEnclosed) rasterEnclosed else rasterFocused))
      g draw p
      g draw r
    }
  }
}