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

package com.siigna.module.cad.modify

import com.siigna._
import app.Siigna
import com.siigna.module.cad.create._

class Move extends Module {

  var startPoint : Option[Vector2D] = None

  protected def toDrawing(p : Vector2D) = p.transform(View.deviceTransformation)
  protected def transformSelection(t : TransformationMatrix) = Drawing.selection.transform(t).shapes.values

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
        startPoint = Some(p)
        // Move the shapes
        'EndPoint
      }

      case End :: tail => End

      case MouseDrag(_, _, _) :: MouseDown(p, _, _) :: tail if Drawing.selection.isDefined => {
        startPoint = Some(p)
        'Drag
      }

      // If a mouse-down is received the user
      //    a) wishes to drag, or
      //    b) Wishes to "pick up" the selection to move it.
      case MouseDown(p : Vector2D, _, _) :: tail => {
        'Drag
      }

      // Special case: Forwarded from Selection
      case Start(_, _) :: End(_) :: MouseDrag(_, _, _) :: MouseDown(p, _, _) :: tail => {
        startPoint = Some(p)
        'Drag
      }

      //on first entry, send input request to input to get the start point for the move operations.
      case e => {
        if (Drawing.selection.isDefined) {
          'StartPoint
        } else {
          Siigna display "nothing selected"
          End
        }
      }
    },

    'StartPoint -> {
      // Exit
      case KeyDown(Key.Esc, _) :: tail => End

      //Vector2D received (from input, could be from mouse up (when input type 8 has been requested),
      // or mouse down (from the other input types used in this module):
      case End(v: Vector2D) :: tail => {
        //The first clicked point can be:
        // a): The point where a drag-move starts, or
        // b): The point, where the user "picks up" the selection that should be moved.
        // That depends on whether the mouse is released at the same, or a different point.
        // So, the input-request is only for a mouse-up (input type 8)
        startPoint = Some(v)

        // Get the endpoint
        'EndPoint
      }

        // Request an input type 6
      case e => Start('cad, "create.InputNew", InputRequestNew(6, None))
    },

    'Drag -> {
      // Exit
      case KeyDown(Key.Esc, _) :: tail => End

      case MouseDrag(p : Vector2D, _, _) :: tail => {
        startPoint match {
            case Some(q) => {
              startPoint = Some(p)
              Drawing.selection.transform(TransformationMatrix(toDrawing(p) - toDrawing(q)))
            }
          case _ =>
        }
      }

      case MouseUp(p : Vector2D, _, _) :: MouseDrag(_, _, _) :: tail => {
        startPoint match {
          case Some(q) => {
            startPoint = Some(p)
            Drawing.selection.transform(TransformationMatrix(toDrawing(p) - toDrawing(q)))
          }
          case _ =>
        }
        End // We're done here
      }

      case e =>
    },

    'EndPoint -> {
      // Exit
      case KeyDown(Key.Esc, _) :: tail => End

      case End(v : Vector2D) :: tail => {
        startPoint match {
          case Some(p) => {
            val t = TransformationMatrix(toDrawing(p) - toDrawing(v))
            Drawing.selection.transform(t)
          }
          case _ => End
        }
      }

      case _ => {
        val inputRequest = InputRequestNew(6,startPoint)
        Start('cad,"create.InputNew", inputRequest)
      }
    }
  )
}