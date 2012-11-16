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
import com.siigna.module.base.create._
import app.model
import model.shape.FullSelector

/**
 * A Module for selecting shapes.
 */
class Selection extends Module {

  private var box : Option[Rectangle2D] = None
  val selectionDistanceSetInSetup = Siigna.double("selectionDistance")

  var nearestShape : Option[(Int, Shape)] = None

  private var selectFullyEnclosed : Boolean = false

  // The starting point of the rectangle
  private var startPoint : Option[Vector2D] = None

  def hasFullShape = {
    if (!nearestShape.isEmpty) {
      Drawing.select(nearestShape.get._1)
      //Start('Move, "com.siigna.module.base.modify")
    }
  }

  def hasPartShape = {
    if (!nearestShape.isEmpty) {
      val shape = nearestShape.get
      val part = shape._2.getPart(mousePosition.transform(View.deviceTransformation))
      Drawing.select(shape._1, part)
    }
  }

  /**
   * Examines whether the selection is currently enclosed or not.
   */
  def isEnclosed : Boolean = startPoint.isDefined && startPoint.get.x <= mousePosition.transform(View.deviceTransformation).x

  def stateMap = Map(
    'Start -> {

      //if ModuleInit forwards to selection with a left mouse click
      // If a shape-part is hit, it is selected:
      case Start(_ ,message : MouseDown) :: tail => {
        val m = mousePosition.transform(View.deviceTransformation)
        //find the shape closest to the mouse:
        if (Drawing(m).size > 0) {
          val nearest = Drawing(m).reduceLeft((a, b) => if (a._2.geometry.distanceTo(m) < b._2.geometry.distanceTo(m)) a else b) 
          if (nearest._2.distanceTo(m) < selectionDistanceSetInSetup.get) {
            nearestShape = Some(nearest)
          } else nearestShape = None
        }
        if (!nearestShape.isEmpty) hasPartShape
        End
      }

      //if ModuleInit forwards to selection with a mouse drag
      // 1: If a shape-part is hit, it is selected (so it will be moved)
      // 2: If not a shape-part is hit, a drag box is initiated:
      case Start(_,message : MouseDrag) :: tail => {
        val m = mousePosition.transform(View.deviceTransformation)
        //find the shape closest to the mouse:
        if (Drawing(m).size > 0) {
          val nearest = Drawing(m).reduceLeft((a, b) => if (a._2.geometry.distanceTo(m) < b._2.geometry.distanceTo(m)) a else b)
          if (nearest._2.distanceTo(m) < selectionDistanceSetInSetup.get) {
            nearestShape = Some(nearest)
          } else nearestShape = None
        }
        if (!nearestShape.isEmpty) {
          hasPartShape
          End
        } else {
        startPoint = Some(message.position)
        'Box
      }}

      //double click anywhere on a shape selects the full shape.
      case Start(_,MouseDouble(p,_,_)) :: tail => {
          startPoint = Some(p)
          val m = mousePosition.transform(View.deviceTransformation)
          //find the shape closest to the mouse:
          if (Drawing(m).size > 0) {
            val nearest = Drawing(m).reduceLeft((a, b) => if (a._2.geometry.distanceTo(m) < b._2.geometry.distanceTo(m)) a else b)
            nearestShape = if (nearest._2.distanceTo(m) < selectionDistanceSetInSetup.get) Some(nearest) else None
          }
          //If a nearest shape was found, this is selected
          if (!nearestShape.isEmpty) {
            hasFullShape
          }
          End
      }

      case MouseMove(_,_,_) :: tail =>
      case f => { println("Selection recieved unkmnown inout: " + f)}
      //

    },

    'Box -> {
      case MouseDrag(p, _, _) :: tail => {
        //Drawing selection box from one side selects all the shapes that "touches" the box
        // from the other only the shapes that are wholly inside the box:,
        if(startPoint.get.x < p.x) {
          selectFullyEnclosed = true
          box = Some(Rectangle2D(startPoint.get, p))
        }
        else {
          selectFullyEnclosed = false
          box = Some(Rectangle2D(startPoint.get, p))
        }
      }
      case MouseUp(p, _, _) :: tail => {
        if (box.isDefined && selectFullyEnclosed == true) {
          Select(box.get.transform(View.deviceTransformation), true)

        }
        //if the selection is drawn from right to left, select partially enclosed shapes as well.:
        else if (box.isDefined && selectFullyEnclosed == false) {
          Select(box.get.transform(View.deviceTransformation), false)
        }
        End
      }
      case e => {
        println("Box ending: " + e)
      }
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