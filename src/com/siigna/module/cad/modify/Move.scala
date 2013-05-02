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

package com.siigna.module.cad.modify

import com.siigna._
import app.Siigna
import com.siigna.module.cad.create._

class Move extends Module {

  var startPoint : Option[Vector2D] = None

  val vector2DGuide = Vector2DGuide((v: Vector2D) => startPoint match {
    case Some(point) => {
      val t : TransformationMatrix = TransformationMatrix(v - point, 1)
      // Return the shape, transformed
      //Drawing.selection.transform(t).shapes.values
      Nil
    }
    case _ => Nil
  })

  def transformSelection(t : TransformationMatrix) = Drawing.selection.transform(t).shapes.values

  val stateMap: StateMap = Map(
    'Start -> {
      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      case MouseDown(p, MouseButtonRight, _) :: tail => End
      case End(KeyDown(Key.Esc, _)) :: tail => End
      case End(MouseDown(p, MouseButtonRight, _)) :: tail => End

      //If the move module starts with a point, it knows where to start...
      case Start(_,p: Vector2D) :: tail => {
        //set the startpoint for the move operation (if not already set)
        startPoint = Some(p.transform(View.deviceTransformation))
        // Move the shapes
        'EndPoint
      }

      case End :: tail => End

      //on first entry, send input request to input to get the start point for the move operations.
      case _ => {
        if (Drawing.selection.isDefined) {
          'StartPoint
        } else {
          Siigna display "nothing selected"
          End
        }
      }
    },

    'StartPoint -> {
      // If a mouse-down is received we assume the user wishes to drag
      case MouseDown(p : Vector2D, _, _) :: tail => {
        'Drag
      }

      //Vector2D received (from input, could be from mouse up (when input type 9 has been requested),
      // or mouse down (from the other input types used in this module):
      case End(v: Vector2D) :: tail => {
        //The first clicked point can be:
        // a): The point where a drag-move starts, or
        // b): The point, where the user "picks up" the selection that should be moved.
        // That depends on whether the mouse is released at the same, or a different point.
        // So, the input-request is only for a mouse-up (input type 9)
        startPoint = Some(v)
        val inputRequest = InputRequest(Some(vector2DGuide),None,None,None,None,None,None,None,None,Some(9))

        // Get the endpoint
        'EndPoint
      }

        // Request an input type 111
      case e => //Start('cad, "create.Input", 111)
    },

    'Drag -> {
      case MouseDrag(p : Vector2D, _, _) :: MouseDown(q, _, _) :: tail => {
        def toDrawing(p : Vector2D) = p.transform(View.deviceTransformation)
        Drawing.selection.transform(TransformationMatrix(toDrawing(p) - toDrawing(q)))
      }

      case MouseUp(p : Vector2D, _, _) :: MouseDrag(_, _, _) :: MouseDown(q, _, _) :: tail => {
        def toDrawing(p : Vector2D) = p.transform(View.deviceTransformation)
        Drawing.selection.transform(TransformationMatrix(toDrawing(p) - toDrawing(q)))
        End // We're done here
      }

      case e => println(e.head)
    },

    'EndPoint -> {
      case End(v : Vector2D) :: tail => {
        //If the received point, when there is a start point, is the same as the start point, it is the start-point for the move
        //(since mouse-up happened on the same spot as mouse-down). Send an input-request for an end-point:
        if (!startPoint.isEmpty && v == startPoint.get) {
          val inputRequest = InputRequest(Some(vector2DGuide),None,None,None,None,None,None,None,None,Some(112))
          Start('cad, "create.Input", inputRequest)
        }

      }

      case _ => {
        val inputRequest = InputRequest(Some(vector2DGuide),None,None,None,None,None,None,None,None,Some(9))
        Start('cad, "create.Input", inputRequest)
        //9 : Input type = KeyUp as input metod,
        //so the coordinates will be returned on key up
        //forward to the Input module with the shape guide.
      }
    }
  )
}