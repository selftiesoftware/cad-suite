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

package com.siigna.module.base

import com.siigna._

/**
 * A Module for selecting shapes.
 */
class Selection extends Module {

  private var box : Option[Rectangle2D] = None

  var nearestShape : Option[(Int, Shape)] = None

  private var selectFullyEnclosed : Boolean = false

  var selectedShape : Option[Shape] = None

  // The starting point of the rectangle
  private var startPoint : Option[Vector2D] = None

  def hasFullShape = {
    if (nearestShape.isDefined) {
      Drawing.select(nearestShape.get._1)
      Start('Move, "com.siigna.module.modify")
    }
  }

  def hasPartShape = {
    if (nearestShape.isDefined) {
      val shape = nearestShape.get
      val part = shape._2.getPart(mousePosition)
      Drawing.select(shape._1, part)
      Start('Move, "com.siigna.module.modify", Drawing.selection.get)
    }
  }

  /**
   * Examines whether the selection is currently enclosed or not.
   */
  def isEnclosed : Boolean = startPoint.isDefined && startPoint.get.x <= mousePosition.x

  def stateMap = Map(
    'Start -> {
      //double click selects over a shape selects the full shape. Double clicks are registered in different ways:
      case MouseDown(p, MouseButtonLeft, _) :: MouseUp(_ ,MouseButtonLeft , _) :: tail => {
        startPoint = Some(p)

        println("nearest defined? "+nearestShape)
        hasFullShape
      }

      case MouseDown(p, _, _) :: tail => {
        println("single mouse down")
        startPoint = Some(p)
        hasPartShape
      }

      case MouseMove(p, _, _) ::  tail => {
        startPoint = Some(p)
        hasPartShape
      }
      case MouseDrag(p, _, _) :: tail => {
        startPoint = Some(p)
        hasPartShape match {
          case m : ModuleInstance => m
          case _ => 'Box
        }
      }
      case MouseUp(_, _, _) :: tail => End
      case e => println(e)
    },
    'Box -> {
      case MouseDrag(p, _, _) :: tail => {
        if(startPoint.get.x < p.x) {
          selectFullyEnclosed = true
          box = Some(Rectangle2D(startPoint.get, p))
        }
        else {
          selectFullyEnclosed = false
          box = Some(Rectangle2D(startPoint.get, p))
        }
      }
      case MouseUp(_, _, _) :: tail => {
        if (box.isDefined && selectFullyEnclosed == true) {
          Select(box.get.transform(View.deviceTransformation), true)
        }
        //if the selection is drawn from right to left, select partially enclosed shapes as well.:
        else if (box.isDefined && selectFullyEnclosed == false) {
          Select(box.get.transform(View.deviceTransformation), false)
        }
        // End the module
        End
      }
      case e => println(e)
    }
  )

  override def paint(g : Graphics, t : TransformationMatrix) {
    val enclosed = "Color" -> "#9999FF".color
    val focused  = "Color" -> "#FF9999".color

    if (box.isDefined) {
      g draw PolylineShape(box.get).setAttribute("Color" -> (if (isEnclosed) enclosed else focused))
    }

  }
}