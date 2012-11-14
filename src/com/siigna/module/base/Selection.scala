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

/**
 * A Module for selecting shapes.
 */
class Selection extends Module {

  private var box : Option[Rectangle2D] = None
  val selectionDistanceSetInSetup = Siigna.double("selectionDistance")

  var nearestShape : Option[(Int, Shape)] = None

  private var selectFullyEnclosed : Boolean = false

  var selectedShape : Option[Shape] = None

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
      //Start('Move, "com.siigna.module.base.modify", Drawing.selection.get)
    }
  }

  /**
   * Examines whether the selection is currently enclosed or not.
   */
  def isEnclosed : Boolean = startPoint.isDefined && startPoint.get.x <= mousePosition.transform(View.deviceTransformation).x

  def stateMap = Map(
    'Start -> {

      //if ModuleInit forwards to selection with a point, it should be used:
      case Start(_ ,p : Vector2D) :: tail => {
        startPoint = Some(p)
        val m = mousePosition.transform(View.deviceTransformation)

        //find the shape closest to the mouse:
        if (Drawing(m).size > 0) {
          val nearest = Drawing(m).reduceLeft((a, b) => if (a._2.geometry.distanceTo(m) < b._2.geometry.distanceTo(m)) a else b) 
          if (nearest._2.distanceTo(m) < selectionDistanceSetInSetup.get) {
            nearestShape = Some(nearest)
          } else nearestShape = None
        }
        if (!nearestShape.isEmpty) {
          println("A nearest shape found at start")
          hasPartShape
          } else {
          println("No nearest shape found at start, select module ends")
          End
        }
      }

      //double click anywhere on a shape selects the full shape.
      /*
      case MouseDown(p1, MouseButtonLeft, _) :: MouseUp(_ ,MouseButtonLeft , _) :: MouseDown(p2, MouseButtonLeft, _) :: tail => {
        println("Doubleclick registered")
        if (p1 == p2) {
        val m = mousePosition.transform(View.deviceTransformation)


        //find the shape closest to the mouse:
        if (Drawing(m).size > 0) {
          val nearest = Drawing(m).reduceLeft((a, b) => if (a._2.geometry.distanceTo(m) < b._2.geometry.distanceTo(m)) a else b)
          nearestShape = if (nearest._2.distanceTo(m) < 5) Some(nearest) else None
        }
        startPoint = Some(p2)
        //hasFullShape
        End
      }} */

      //Single leftclick selects nearest shapepart:
      case MouseDown(p, MouseButtonLeft, _) :: tail => {
        startPoint = Some(p)
        val m = mousePosition.transform(View.deviceTransformation)
        nearestShape = None
        
        //find the shape closest to the mouse:
        if (Drawing(m).size > 0) {
          val nearest = Drawing(m).reduceLeft((a, b) => if (a._2.geometry.distanceTo(m) < b._2.geometry.distanceTo(m)) a else b)
          nearestShape = if (nearest._2.distanceTo(m) < selectionDistanceSetInSetup.get) Some(nearest) else None
        }
        //If a nearest shape was found, the module continues, else it ends
        if (!nearestShape.isEmpty) {
          println("A nearest shape found by singleclick")
          println("nearestShape: " + nearestShape)

          hasPartShape
        } else {
          println("No nearest shape found by singleclick, module ends")
          Drawing.deselect()
          End
        }

      }

      /*
      case MouseMove(p, _, _) ::  tail => {
        startPoint = Some(p)
        //hasPartShape
      }
      case MouseDrag(p, _, _) :: tail => {
        startPoint = Some(p)
        /*hasPartShape match {
          case m : ModuleInstance => m
          case _ => 'Box
        } */
      } */

      // Exit strategy
      case KeyDown(Key.Esc, _) :: tail => End

      case e => {
        //println(e)
      }

      //

    }/*,

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
      case e => {

        println(e)
      }
    } */
  )

  override def paint(g : Graphics, t : TransformationMatrix) {
    val enclosed = "Color" -> "#9999FF".color
    val focused  = "Color" -> "#FF9999".color

    if (box.isDefined) {
      g draw PolylineShape(box.get).setAttribute("Color" -> (if (isEnclosed) enclosed else focused))
    }

  }
}