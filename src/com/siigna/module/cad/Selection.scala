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
/**
 * A Module for selecting shapes.
 */
class Selection extends Module {

  private var box : Option[SimpleRectangle2D] = None

  var nearestShape : Option[(Int, Shape)] = None

  private var selectEntireShape : Boolean = false

  // The starting point of the rectangle
  private var startPoint : Option[Vector2D] = None
  private var zoom : Double = View.zoom

   //find the shape closest to the mouse
  def findNearest(m : Vector2D) = {
    var n : Option[(Int, Shape)] = None
    if (Drawing(m).size > 0) {
      val nearest = Drawing(m).reduceLeft((a, b) => if (a._2.geometry.distanceTo(m) < b._2.geometry.distanceTo(m)) a else b)
      n = if (nearest._2.distanceTo(m) < Siigna.selectionDistance) Some(nearest) else None
    }
    n
  }
  //test if the segment or point is selected
  //Return True if the segment is selected
  //Return False if the segment is not selected
  def isSelected(m : Vector2D) : Boolean = {
    val clickedShp = findNearest(m)
    if(Drawing.selection.isDefined && clickedShp.isDefined) {
      val parts = Drawing.selection.get.shapes
      parts.foreach(s => println("AA: "+s._1))
      println(clickedShp.get._1)
    }
    false
  }

  /**
   * Examines whether the selection is currently enclosed or not.
   */
  def isEnclosed : Boolean = startPoint.isDefined && startPoint.get.x <= mousePosition.transform(View.deviceTransformation).x

  var hasShift = false

  //select or deselect part shapes
  def processPartShape(m: Vector2D) = {
    nearestShape = findNearest(m)   //find the shape closest to the mouse
    //if a nearest shape is defined, and SHIFT is not pressed, add the part to the selection
    if (!nearestShape.isEmpty && hasShift == false) {
      val part = nearestShape.get._2.getPart(mousePosition.transform(View.deviceTransformation))
      Drawing.select(nearestShape.get._1, part)
    } else if (!nearestShape.isEmpty && hasShift == true && isSelected(m) == false) {
      //TODO: update this so that it deselects only a segment, not the entire model.
      Drawing.deselect()
    }
  }

  //select or deselect full shapes
  def processFullShape(m: Vector2D) = {
    nearestShape = findNearest(m) //find the shape closest to the mouse
    //if a nearest shape is defined, and SHIFT is not pressed, add the part to the selection
    if (!nearestShape.isEmpty && hasShift == false) {
      Drawing.select(nearestShape.get._1)
    } else if (!nearestShape.isEmpty && hasShift == true) {
      //TODO: update this so that is deselects only the relevant shape, not all shapes.
      Drawing.deselect()
    }
  }

  def stateMap = Map(
  'Start -> {

      //case events => println(events)
      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End

      //check for SHIFT down /up events
      case Start(_ , _) :: KeyDown(Key.Shift, _) :: tail => hasShift = true
      case KeyDown(Key.Shift, _) :: tail => hasShift = true

      //if ModuleInit forwards to selection with a left mouse click
      // If a shape-part is hit, it is selected:
      case Start(_ ,message : MouseDown) :: tail => {
        val m = mousePosition.transform(View.deviceTransformation)
        processPartShape(m)
        End
      }

      //if SHIFT is pressed the selection module does not end, but listens after events.
      //in case of a mouse down, a de-selection is made.

      case MouseDown(p, _, _) :: tail => {
        val m = mousePosition.transform(View.deviceTransformation)
        processPartShape(m)
        End
      }

      case MouseUp(p, _, _) :: tail => {
        val m = mousePosition.transform(View.deviceTransformation)
        processPartShape(m)
        End
      }
      //if ModuleInit forwards to selection with a mouse drag
      // 1: If a shape-part is hit, it is selected (so it will be moved)
      // 2: If not a shape-part is hit, a drag box is initiated:
      case Start(_,message : MouseDrag) :: tail => {
        val m = mousePosition.transform(View.deviceTransformation)

        //find the shape closest to the mouse:
        nearestShape = findNearest(m)

        if (!nearestShape.isEmpty) {
          processPartShape(m)
          End
        } else {
        startPoint = Some(message.position)
        'Box
      }}

      //double click anywhere on a shape selects the full shape.
      case Start(_,MouseDouble(p,_,_)) :: tail => {
        val m = mousePosition.transform(View.deviceTransformation)
        processFullShape(m)
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
        //Dragging a selection box from LEFT TO RIGHT: ONLY shapes that are fully enclosed in the box are selected.
        if(startPoint.get.x < p.x) {
          selectEntireShape = false
          box = Some(Rectangle2D(startPoint.get, p))
        }
        //Dragging from RIGHT TO LEFT selects all the shapes that intersect the box
        else {
          selectEntireShape = true
          box = Some(Rectangle2D(startPoint.get, p))
        }
      }
      case MouseUp(p, _, _) :: tail => {
        if (box.isDefined && selectEntireShape == true) Select(box.get.transform(View.deviceTransformation), true)
        else if (box.isDefined && selectEntireShape == false) Select(box.get.transform(View.deviceTransformation), false)
        End
      }
      case e => {
        println("Box ending: cannot select this...")
        End
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