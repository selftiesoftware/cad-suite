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

  def m = mousePosition.transform(View.deviceTransformation)

  def nearestShape : Option[(Int, Shape)] = {
    val drawing = Drawing(m)
    if (!drawing.isEmpty) {
      Some(drawing.reduceLeft((a, b) => if (a._2.distanceTo(m) < a._2.distanceTo(m)) a else b))
    } else None
  }

  def shapeWithinSelectionDistance: Boolean = {
    if (nearestShape.isDefined) {
      if (nearestShape.get._2.distanceTo(m) < Siigna.selectionDistance) true
        else false
    } else {
      false
    }
  }

  def stateMap = Map(
  'Start -> {

    //exit strategy
    case KeyDown(Key.Esc,m) :: tail => End(KeyDown(Key.Escape,m))
    case MouseDown(p, MouseButtonRight,m) :: tail => End(MouseDown(p,MouseButtonRight,m))

    //Mouse up, after mouse down - possible double click:
    case MouseUp(p1, _, m1) :: Start(_ , p2: Vector2D) :: MouseDown(p3,MouseButtonLeft,m3) :: MouseUp(p4, _, m4) :: MouseDown(p5,MouseButtonLeft,m5) :: tail
      if (p1 == p5) => {
      println("0: " + tail)
      if(!m1.shift) {
        //If shift is not down, and clicking away from shape: Deselect any selections.
        if (!shapeWithinSelectionDistance) Deselect()
        //If clicking near shapes, and on the same point as last mouse up: Select whole shape (Doubleclick)
        //TOTO: Insert time factor
        else {
          val (id, shape) = nearestShape.get
          Deselect()
          Select(id)
          End
        }
      } else {
        //If shift is down, and clicking near shape, and it is the same place as before: Doubleclick = toggle selection:
        val (id, shape) = nearestShape.get
        SelectToggle(id)
        End
      }

    }

    //If started with a Vector2D: It's a MouseDown
    case Start(_ , p1: Vector2D) :: MouseDown(p2,MouseButtonLeft,m2) :: tail => {
      println("1: " + tail)
      if (shapeWithinSelectionDistance) {
        //If near shape part: Toggle, if shift is down, select if shift is not down
        //If not near shape part: Do nothing (before the mouse button is released again)
        if (nearestShape.isDefined) {
          val (id, shape) = nearestShape.get
          val selector = shape.getSelector(m)
          //If shift is down, toggle selection of nearest shape part (RESTORE THIS WHEN SHIFT REPEAT IS GOne froM EVENTSTREAM):
          //if (m2.shift) SelectToggle(id,selector)
          //If shift is down, and clicking near shape, and it is the same place as before: Doubleclick = toggle selection:
          //This is for using single click + shift as doubleclick, since shift repeats and corrupts eventstream, and doesnt work for now...
          if (m2.shift) {
          SelectToggle(id)
          End}
          //If shift is not down, deselect anything that might be selected, and select nearest shape part:
          else {
            Deselect()
            Select(id,selector)
          }
        }
      } else if (!m2.shift) Deselect()
    }

    //If started with a Vector2D: It's a MouseDown
    //Sometimes, there is an extra "End"... Otherwise the same as above.
    case Start(_ , p1: Vector2D) :: End(_) :: MouseDown(p2,MouseButtonLeft,m2) :: tail => {
      println("1: " + tail)
      if (shapeWithinSelectionDistance) {
        //If near shape part: Toggle, if shift is down, select if shift is not down
        //If not near shape part: Do nothing (before the mouse button is released again)
        if (nearestShape.isDefined) {
          val (id, shape) = nearestShape.get
          val selector = shape.getSelector(m)
          //If shift is down, toggle selection of nearest shape part:
          if (m2.shift) SelectToggle(id,selector)
          //If shift is not down, deselect anything that might be selected, and select nearest shape part:
          else {
            Deselect()
            Select(id,selector)
          }
        }
      } else if (!m2.shift) Deselect()
    }

    //If started without a point, catch mouse down:
    case MouseDown(p1,MouseButtonLeft,m1) :: tail => {
      println("4: " + tail)
      if (shapeWithinSelectionDistance) {
        //If near shape part: Toggle, if shift is down, select if shift is not down
        //If not near shape part: Do nothing (before the mouse button is released again)
        if (nearestShape.isDefined) {
          val (id, shape) = nearestShape.get
          val selector = shape.getSelector(m)
          //If shift is down, toggle selection of nearest shape part (RESTORE THIS WHEN SHIFT REPEAT IS GOne froM EVENTSTREAM):
          //if (m2.shift) SelectToggle(id,selector)
          //If shift is down, and clicking near shape, and it is the same place as before: Doubleclick = toggle selection:
          //This is for using single click + shift as doubleclick, since shift repeats and corrupts eventstream, and doesnt work for now...
          if (m1.shift) {
            SelectToggle(id)
            End}
          //If shift is not down, deselect anything that might be selected, and select nearest shape part:
          else {
            Deselect()
            Select(id,selector)
          }
        }
      } else if (!m1.shift) Deselect()
    }

    //Click, then drag:
    case MouseDrag(p1,MouseButtonLeft,m1) :: tail
      //Shift down: Box selUect, toggle:
      if (m1.shift) => {
        println("2")
        startPoint = Some(p1)
        'Box
      }
    case MouseDrag(p1,MouseButtonLeft,m1) :: tail
      //Shift up: Drag move if near shape, box select if not near shape:
      if (!m1.shift) => {
      println("3")
      println(tail)
      if (shapeWithinSelectionDistance) {
        End(Module('cad, "modify.Move"))
      }
      else {
        startPoint = Some(p1)
        'Box
      }
      }





    /*
    //If double-click:
    case MouseUp(p5, _, modifier5) :: Start(_ , p0: Vector2D) :: MouseDown(p1, _, modifier1) :: MouseUp(p2, _, modifier2) :: MouseDown(p3, _, modifier3) :: tail
      if (p1 == p3 && System.currentTimeMillis() - SelectionTime.time < 500 && SelectionTime.firstClick == false) => {
      println("HEREEEE")
        val m = mousePosition.transform(View.deviceTransformation)
        // A value defining whether there is a shape within selection distance
        lazy val shapeinDrawingWithinSelectionDistance: Boolean = {
          val m = mousePosition.transform(View.deviceTransformation)
          val drawing = Drawing(m)
          if ((drawing.reduceLeft((a, b) => if (a._2.distanceTo(m) < a._2.distanceTo(m)) a else b))._2.distanceTo(m) < Siigna.selectionDistance) true
          else false
        }


        // A lazy value for finding the closes shape in the drawing - might not be used
        lazy val drawingShape : Option[(Int, Shape)] = {
          val drawing = Drawing(m)
          if (!drawing.isEmpty) {
            Some(drawing.reduceLeft((a, b) => if (a._2.distanceTo(m) < a._2.distanceTo(m)) a else b))
          } else None
        }
        //If shift is down, toggle shape within selection distance
          val nearestShape : Option[(Int, Shape)] = if (Drawing.selection.isDefined) {
            val x = Drawing.selection.reduceLeft((a, b) => if (a._2._1.distanceTo(m) < a._2._1.distanceTo(m)) a else b)
            x._2._1.getSelector(m) match {
              case EmptyShapeSelector => drawingShape
              case _ => Some(x._1 -> x._2._1)
            }
          } else drawingShape
          if (modifier1 == Key.Shift) {
            //Toggles selection if there is a neaarest shape.
            nearestShape.map(t => SelectToggle(t._1)).getOrElse(Unit)
          } else {
            nearestShape.map(t => Select(t._1)).getOrElse(Unit)
          }
    }

    //If started with a Vector2D:
    case Start(_ , v: Vector2D) :: tail => {
      println("Case Start")

    }

    // If we immediately receive a mouse-up we select nearby shapes
    case MouseUp(p, _, modifier) :: tail => {
      println("Time set")
      if (System.currentTimeMillis() - SelectionTime.time > 500) {
        SelectionTime.time = System.currentTimeMillis()
        SelectionTime.firstClick = true
      } else SelectionTime.firstClick = false
      val m = mousePosition.transform(View.deviceTransformation)
      // If shift is not pressed we should deselect
      if (!modifier.shift) {
        // A lazy value for finding the closes shape in the drawing - might not be used
        lazy val drawingShape : Option[(Int, Shape)] = {
          val drawing = Drawing(m)
          if (!drawing.isEmpty) {
            Some(drawing.reduceLeft((a, b) => if (a._2.distanceTo(m) < a._2.distanceTo(m)) a else b))
          } else None
        }

        // First examine if a shape, that is already close to the point, is selected. If so, we should ignore other shapes
        // Fixes trello: http://goo.gl/Cdlyc
        val nearestShape : Option[(Int, Shape)] = if (Drawing.selection.isDefined) {
          val x = Drawing.selection.reduceLeft((a, b) => if (a._2._1.distanceTo(m) < a._2._1.distanceTo(m)) a else b)
          x._2._1.getSelector(m) match {
            case EmptyShapeSelector => drawingShape
            case _ => Some(x._1 -> x._2._1)
          }
        } else drawingShape

        // Select the shape, if available
        if (nearestShape.isDefined) {
          val (id, shape) = nearestShape.get
          val selector = shape.getSelector(m)
          Drawing.selection.get(id) match {
            // If we can find the exact same selector, select the entire shape (equal to dbl-click)
            case Some((x, y)) if y == selector && y != FullShapeSelector => Select(id)
            // If the old selector differs we simply select the new selection
            case _ if shape.distanceTo(m) < Siigna.selectionDistance => {
              Deselect() // Deselect the current selection since shift is up
              Select(id, selector)
            }
            case _ => Deselect() // Deselect the current selection since shift is up
          }
        } else Deselect() // Deselect the current selection since shift is up
      } else {
        // Select the area
        SelectToggle(m)
      }

      End
    }

    // Store selections close to the mouse-point for visual feedback, before any actions are taken
    case MouseMove(p, _, keys) :: tail => {
      val point = p.transform(View.deviceTransformation)
      val shapes = Drawing(point)
      activeSelection = Selection(shapes.map(t => t._1 -> (t._2 -> t._2.getSelector(point))))
    }

    // Forward to Move if we start to drag and a selection is active and shift is not down
    case MouseDrag(_, _, mod) :: Start(_, _) :: End(_) :: MouseDown(p, _, _) :: tail => {
      println("caseMouseDrag")
      val mouse = p.transform(View.deviceTransformation)
      Select(mouse)

      // If one or more shapes have been selected and the cursor is close to a shape, assume we want to drag-move
      if (!Drawing.selection.isEmpty && !Drawing(mouse).isEmpty) {
        End(Module('cad, "modify.Move"))
      // If no shape are selected or close, assume the user wants a box-selection
      } else {
        startPoint = Some(p)
        'Box
      //}
      } }

    //When coming from module as for instance copy, when there is no selection, this event-list is needed to start box-selection:
      case MouseDrag(_, _, mod) :: MouseDown(p, _, _) :: tail => {
        println("Case mouse drag 2")
        startPoint = Some(p)
        'Box
      }                 */


    // Do nothing if shift is pressed, since it repeats when held down - ignore that...
    case KeyDown(Key.Shift, _) :: tail =>

    //Do not exit when starting without point:
    case Start(_, _) :: tail =>

    //Also do not exit on mouse move - it can cause endless loops and hang the programme
    case MouseMove(_,_,_) :: tail =>

    case x => {
      println("99: " + x)
      End
    }

  },

  'Box -> {
    //exit strategy
    case KeyDown(Key.Esc,m) :: tail => End(KeyDown(Key.Escape,m))
    case MouseDown(p, MouseButtonRight,m) :: tail => End(MouseDown(p,MouseButtonRight,m))





    // Do nothing if shift is pressed
    case KeyDown(Key.Shift, _) :: tail =>

    case MouseDrag(p, _, _) :: tail => {
      println("01")
      val rectangle = Rectangle2D(startPoint.get, p)
      box = Some(rectangle)
      val transformedRectangle = rectangle.transform(View.deviceTransformation)
      val selection = Drawing(transformedRectangle).map(t =>
        t._1 -> (t._2 -> (if (isEnclosed) FullShapeSelector else t._2.getSelector(transformedRectangle))))
      activeSelection = Selection(selection)
    }


    case MouseUp(p, _, keys) :: tail => {
      println("02")
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



    case e => println(e)

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

private object SelectionTime {
  var firstClick : Boolean = true
  var time : Long = System.currentTimeMillis()

}