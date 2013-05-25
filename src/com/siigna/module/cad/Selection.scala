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

package com.siigna.module.cad

import com.siigna._
import com.siigna.{Selection => SiignaSelection}
import java.awt.Color
import com.siigna.app.model.selection.EmptySelection

/**
 * A Module for selecting shapes and shape-parts.
 */
class Selection extends Module {

  // The box, describing the area selection, if any
  private var box : Option[SimpleRectangle2D] = None

  // The selection from the box, if any
  private var activeSelection : SiignaSelection = EmptySelection

  // The starting point of the box-selection
  private var startPoint : Option[Vector2D] = None

  /**
   * Examines whether the selection is currently enclosed (selects the entire shapes) or not (only selects parts).
   */
  def isEnclosed : Boolean = startPoint.isDefined && startPoint.get.x <= mousePosition.x

  def stateMap = Map(
  'Start -> {

    //exit strategy
    case KeyDown(Key.Esc, _) :: tail => End
    case MouseUp(_, MouseButtonRight, _) :: tail => End

    // If we immediately receive a mouse-up we select nearby shapes
    case MouseUp(p, _, modifier) :: tail => {
      val m = mousePosition.transform(View.deviceTransformation)
      // If shift is not pressed we should deselect everything
      if (!modifier.shift) {
        Drawing.deselect()
      }

      // Select the area
      SelectToggle(m)

      End
    }

    //double click anywhere on a shape selects the full shape.
    case Start :: MouseUp(_, _, _) :: MouseDown(_, _, _) :: MouseUp(_, _, _) :: tail => {
      val m = mousePosition.transform(View.deviceTransformation)
      Select(Drawing(m), m)
      End
    }

    // Store selections close to the mouse-point for visual feedback, before any actions are taken
    case MouseMove(p, _, keys) :: tail => {
      val point = p.transform(View.deviceTransformation)
      val shapes = Drawing(point)
      activeSelection = Selection(shapes.map(t => t._1 -> (t._2 -> t._2.getSelector(point))))
    }

    // Forward to box if the mouse is dragged
    case (m : MouseDrag) :: tail => {
      startPoint = Some(m.position)
      'Box
    }

    // Forward to drag if we receive a mouse down with a shape close-by
    case MouseDown(p, MouseButtonLeft, _) :: tail => {
      val mouse = p.transform(View.deviceTransformation)
      val shapes = Drawing(mouse)
      if (!shapes.isEmpty) {
        val (id, shape) = shapes.reduceLeft((a, b) => if (a._2.distanceTo(mouse) < b._2.distanceTo(mouse)) a else b)
        shape.getSelector(mouse) match {
          case EmptyShapeSelector =>
          case x => Select(id, shape, x)
        }
      }

      // Forward to
    }

  },

  'Box -> {
    //exit strategy
    case KeyDown(Key.Esc, _) :: tail => End
    case MouseDown(p, MouseButtonRight, _) :: tail => End

    // Do nothing if shift is pressed
    case KeyDown(Key.Shift, _) :: tail =>

    case MouseDrag(p, _, _) :: tail => {
      val rectangle = Rectangle2D(startPoint.get, p)
      box = Some(rectangle)
      val transformedRectangle = rectangle.transform(View.deviceTransformation)
      val selection = Drawing(transformedRectangle).map(t =>
        t._1 -> (t._2 -> (if (isEnclosed) FullShapeSelector else t._2.getSelector(transformedRectangle))))
      activeSelection = Selection(selection)
    }
    case MouseUp(p, _, keys) :: tail => {
      if (box.isDefined) {
        // Toggle the selection if shift is pressed
        if (keys == Shift) {
          SelectToggle(box.get.transform(View.deviceTransformation), isEnclosed)
        } else {
          Drawing.deselect()
          Select(box.get.transform(View.deviceTransformation), isEnclosed)
        }
      }
      End
    }
    case e => End
  },


  'Drag -> {
    case _ =>
  })

  private val highlighted = "#554499".color
  private val enclosed    = "#8899CC".color
  private val focused     = "#CC8899".color
  private val rasterEnclosed = new Color(100, 120, 210, 20)
  private val rasterFocused  = new Color(210, 100, 120, 20)

  override def paint(g : Graphics, t : TransformationMatrix) {

    if (box.isDefined) {
      val p = PolylineShape(box.get).setAttribute("Color" -> (if (isEnclosed) enclosed else focused))
      val r = p.setAttributes("Raster" -> (if (isEnclosed) rasterEnclosed else rasterFocused))
      g draw p
      g draw r
    }

    activeSelection.parts.foreach(p => {
      g draw p.setAttributes("Color" -> highlighted).transform(t)
    })
    activeSelection.vertices.foreach(v => g draw v.transform(t))
  }
}