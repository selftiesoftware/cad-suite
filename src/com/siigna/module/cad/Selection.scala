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
  private var boxSelection : SiignaSelection = EmptySelection

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

    case MouseUp(p, _, modifier) :: tail => {
      val m = mousePosition.transform(View.deviceTransformation)
      // If shift is not pressed we should deselect everything
      if (!modifier.shift) {
        Drawing.deselect()
      }

      // Select the area
      Select(Drawing(m), m)

      End
    }

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
  },

  'Box -> {
    //exit strategy
    case KeyDown(Key.Esc, _) :: tail => End
    case MouseDown(p, MouseButtonRight, _) :: tail => End

    case MouseDrag(p, _, _) :: tail => {
      val rectangle = Rectangle2D(startPoint.get, p)
      box = Some(rectangle)
      val transformedRectangle = rectangle.transform(View.deviceTransformation)
      val selection = Drawing(transformedRectangle).map(t =>
        t._1 -> (t._2 -> (if (isEnclosed) FullShapeSelector else t._2.getSelector(transformedRectangle))))
      boxSelection = Selection(selection)
    }
    case MouseUp(p, _, _) :: tail => {
      if (box.isDefined) {
        Select(box.get.transform(View.deviceTransformation), isEnclosed)
      }
      End
    }
    case e => End
  })

  private val highlighted = "#667788".color
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

    boxSelection.parts.foreach(p => {
      g draw p.setAttributes("Color" -> highlighted).transform(t)
    })
    boxSelection.vertices.foreach(v => g draw v.transform(t))
  }
}