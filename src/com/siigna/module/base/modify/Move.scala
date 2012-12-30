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

package com.siigna.module.base.modify

import com.siigna._
import app.Siigna
import com.siigna.module.base.create._
import module.ModuleInit

class Move extends Module {
  var endPoint : Option[Vector2D] = None
  var startPoint : Option[Vector2D] = None

  var transformation : Option[TransformationMatrix] = None

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
        //definition of a shape guide that is used to send the selected shapes to the 'Point module
        // where they are drawn dynamically
        val vector2DGuide = Vector2DGuide((v: Vector2D) => {
          val t : TransformationMatrix = if (startPoint.isDefined) {
            TransformationMatrix(v - startPoint.get, 1)
            // If no startPoint has been defined - create an empty matrix
          } else TransformationMatrix()
          // Return the shape, transformed
          Drawing.selection.get.apply(t)
        })
        val inputRequest = InputRequest(Some(vector2DGuide),None,None,None,None,None,None,None,None,Some(9))
        Start('Input, "com.siigna.module.base.create",inputRequest)
        //9 : Input type = KeyUp as input metod,
        //so the coordinates will be returned on key up
        //forward to the Input module with the shape guide.
      }

      //Vector2D received (from input, could be from mouse up (when input type 9 has been requested),
      // or mouse down (from the other input types used in this module):
      case End(v: Vector2D) :: tail => {
        if (startPoint.isEmpty) {
          //The first clicked point can be:
          // a): The point where a drag-move starts, or
          // b): The point, where the user "picks up" the selection that should be moved.
          // That depends on whether the mouse is released at the same, or a different point.
          // So, the input-request is only for a mouse-up (input type 9)
          startPoint = Some(v)
          val vector2DGuide = Vector2DGuide((v: Vector2D) => {
            val t : TransformationMatrix = TransformationMatrix(v - startPoint.get, 1)
            // Return the shape, transformed
            Drawing.selection.get.apply(t)
          })
          val inputRequest = InputRequest(Some(vector2DGuide),None,None,None,None,None,None,None,None,Some(9))
          Start('Input, "com.siigna.module.base.create",inputRequest)
        } else if (!startPoint.isEmpty && v == startPoint.get) {
          //If the received point, when there is a start point, is the same as the start point, it is the start-point for the move
          //(since mouse-up happened on the same spot as mouse-down). Send an input-request for an end-point:
          val vector2DGuide = Vector2DGuide((v: Vector2D) => {
            val t : TransformationMatrix = TransformationMatrix(v - startPoint.get, 1)
            // Return the shape, transformed
            Drawing.selection.get.apply(t)
          })
          val inputRequest = InputRequest(Some(vector2DGuide),None,None,None,None,None,None,None,None,Some(112))
          Start('Input, "com.siigna.module.base.create",inputRequest)
        } else if (!startPoint.isEmpty) {
          //If the received point, when there is a start point, is NOT the same as the start point, it is the end-point for the move
          //(since mouse-up, or mouse down, happened on a different spot than the start point). Do the move:
          transformation = Some(TransformationMatrix((v - startPoint.get), 1))
          Drawing.selection.get.transform(transformation.get)
          Drawing.deselect()
          End
        }
      }

      case End :: tail => End

      //on first entry, send input request to input to get the start point for the move operations.
      case _ => {
        //Should be done differently, but this is how I can reach this (usableSelectionExists) function just quickly...
        val l = new ModuleInit
        if (l.usableSelectionExists) {
          Start('Input, "com.siigna.module.base.create",111)
        } else {
          Siigna display "nothing selected"
         End
        }
      }
    }
  )
}