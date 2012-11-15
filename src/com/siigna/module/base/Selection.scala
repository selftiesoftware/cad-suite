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
    }
  }

  /**
   * Examines whether the selection is currently enclosed or not.
   */
  def isEnclosed : Boolean = startPoint.isDefined && startPoint.get.x <= mousePosition.transform(View.deviceTransformation).x

  def stateMap = Map(
    'Start -> {

      //if ModuleInit forwards to selection with a left mouse click:
      case Start(_ ,message : MouseDown) :: tail => {
        startPoint = Some(message.position)
        val m = mousePosition.transform(View.deviceTransformation)
        //find the shape closest to the mouse:
        if (Drawing(m).size > 0) {
          val nearest = Drawing(m).reduceLeft((a, b) => if (a._2.geometry.distanceTo(m) < b._2.geometry.distanceTo(m)) a else b) 
          if (nearest._2.distanceTo(m) < selectionDistanceSetInSetup.get) {
            nearestShape = Some(nearest)
          } else nearestShape = None
        }

        if (!nearestShape.isEmpty) hasPartShape

        if (Drawing.selection.isEmpty) {
          End
        }
      }

      //if ModuleInit forwards to selection with a mouse drag:
      case Start(_,message : MouseDrag) :: tail => {
        startPoint = Some(message.position)
        'Box
      }

      //double click anywhere on a shape selects the full shape.
      case MouseDown(p1, MouseButtonLeft, _) :: MouseUp(_ ,MouseButtonLeft , _) :: MouseDown(p2, MouseButtonLeft, _) :: tail => {
        if (p1 == p2) {
          startPoint = Some(p2)
          val m = mousePosition.transform(View.deviceTransformation)
          //find the shape closest to the mouse:
          if (Drawing(m).size > 0) {
            val nearest = Drawing(m).reduceLeft((a, b) => if (a._2.geometry.distanceTo(m) < b._2.geometry.distanceTo(m)) a else b)
            nearestShape = if (nearest._2.distanceTo(m) < 5) Some(nearest) else None
          }
          //If a nearest shape was found, this is selected
          if (!nearestShape.isEmpty) {
            hasFullShape
            //If nothing is selected, the module ends
          }
          if (Drawing.selection.isEmpty) {
            End
          }
      }}

      //Single leftclick selects nearest shapepart, if nothing is selected yet:
      case MouseDown(p, MouseButtonLeft, _) :: tail => {
        if (Drawing.selection.isEmpty) {
          startPoint = Some(p)
          val m = mousePosition.transform(View.deviceTransformation)
          nearestShape = None
          //find the shape closest to the mouse:
          if (Drawing(m).size > 0) {
            val nearest = Drawing(m).reduceLeft((a, b) => if (a._2.geometry.distanceTo(m) < b._2.geometry.distanceTo(m)) a else b)
            nearestShape = if (nearest._2.distanceTo(m) < selectionDistanceSetInSetup.get) Some(nearest) else None
          }
          //If a nearest shape was found, this is selected
          if (!nearestShape.isEmpty) {
            hasPartShape
          //If nothing is selected, the module ends
          }
        }
        //If there still is nothing selected, the module ends
        if (Drawing.selection.isEmpty) {
          End
        }
      }

      //If a drag is performed, and nothing is selected, something should be selected.
      //If something is selected, it should be moved.
      case MouseDrag(p,_,_) :: tail => {
        if (Drawing.selection.isEmpty) {
          startPoint = Some(p)
          'Box
        } else {
          Start('Move, "com.siigna.module.base.modify", p)
        }
      }

      // Delete
      case KeyDown(Key.Delete, _) :: tail => {
        if (!Drawing.selection.isEmpty) {
          println("Insert delete module here")
          Delete(Drawing.selection.get.self)
          val ss = new Delete
        }
      }

      // Delete
      case KeyDown(Key.Backspace, _) :: tail => {
        if (!Drawing.selection.isEmpty) {
          println("Insert delete module here")
          Delete(Drawing.selection.get.self)
        }
      }
        
      // Exit strategy
      case KeyDown(Key.Esc, _) :: tail => {
        Drawing.deselect()
        End
      }
      case MouseDown(_,MouseButtonRight,_)::tail => {

        Drawing.deselect()

        End
      }

      case End :: tail =>        
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
        //Return from this part
        box = None
        'Start
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