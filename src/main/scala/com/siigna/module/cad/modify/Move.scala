/*
* Copyright (c) 2008-2013. Siigna is released under the creative common license by-nc-sa. You are free
* to Share — to copy, distribute and transmit the work,
* to Remix — to adapt the work
*
* Under the following conditions:
* Attribution — You must attribute the work to http://siigna.com in the manner specified by the author or licensor (but not in any way that suggests that they endorse you or your use of the work).
* Noncommercial — You may not use this work for commercial purposes.
* Share Alike — If you alter, transform, or build upon this work, you may distribute the resulting work only under the same or similar license to this one.
*/

package com.siigna.module.cad.modify

import com.siigna._
import app.Siigna
import com.siigna.module.cad.create._

class Move extends Module {
  val origin = Drawing.selection.transformation

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
          Drawing.selection.transformation = origin
          val t : TransformationMatrix = if (startPoint.isDefined) {
            TransformationMatrix(v - startPoint.get, 1)
            // If no startPoint has been defined - create an empty matrix
          } else TransformationMatrix()
          // Return the shape, transformed TODO: if else here is a hack to prevent none.get error if selection is empty
          if(!Drawing.selection.isEmpty) Drawing.selection.transform(t).shapes.values else Traversable(LineShape(Vector2D(0,0),Vector2D(10,10)))
        })
        val inputRequest = InputRequest(8,None,vector2DGuide)
        Start('cad, "create.Input", inputRequest)
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
            Drawing.selection.transformation = origin
            val t : TransformationMatrix = TransformationMatrix(v - startPoint.get, 1)
            // Return the shape, transformed
            Drawing.selection.transform(t).shapes.values
          })
          val inputRequest = InputRequest(8,None,vector2DGuide)
          Start('cad, "create.Input", inputRequest)
        } else if (!startPoint.isEmpty && v == startPoint.get) {
          //If the received point, when there is a start point, is the same as the start point, it is the start-point for the move
          //(since mouse-up happened on the same spot as mouse-down). Send an input-request for an end-point:
          val vector2DGuide = Vector2DGuide((v: Vector2D) => {
            Drawing.selection.transformation = origin
            val t : TransformationMatrix = TransformationMatrix(v - startPoint.get, 1)
            // Return the shape, transformed
            Drawing.selection.transform(t).shapes.values
          })
          val inputRequest = InputRequest(5,None,vector2DGuide)
          Start('cad, "create.Input", inputRequest)
        } else if (!startPoint.isEmpty) {
          //If the received point, when there is a start point, is NOT the same as the start point, it is the end-point for the move
          //(since mouse-up, or mouse down, happened on a different spot than the start point). Do the move:
          Drawing.selection.transformation = origin
          transformation = Some(TransformationMatrix((v - startPoint.get), 1))
          Drawing.selection.transform(transformation.get)
          Drawing.deselect()
          End
        }
      }

      //Input Type 16 returns mouse-down-event, if a vector is typed by key - if such a vector is typed, do the move:
      case End(MouseDown(v: Vector2D,_,_)) :: tail => {
        Drawing.selection.transformation = origin
        transformation = Some(TransformationMatrix(v, 1))
        Drawing.selection.transform(transformation.get)
        Drawing.deselect()
        End
      }

      case End :: tail => End

      case Start (_,_) :: End(_) :: MouseDrag(_, _, mod) :: MouseDown(p, _, _) :: tail => {
        //set the startpoint for the move operation (if not already set)
        println("DragMove begins in move")
        startPoint = Some(p.transform(View.deviceTransformation))
        //definition of a shape guide that is used to send the selected shapes to the 'Point module
        // where they are drawn dynamically
        val vector2DGuide = Vector2DGuide((v: Vector2D) => {
          Drawing.selection.transformation = origin
          val t : TransformationMatrix = if (startPoint.isDefined) {
            TransformationMatrix(v - startPoint.get, 1)
            // If no startPoint has been defined - create an empty matrix
          } else TransformationMatrix()
          // Return the shape, transformed TODO: if else here is a hack to prevent none.get error if selection is empty
          if(!Drawing.selection.isEmpty) Drawing.selection.transform(t).shapes.values else Traversable(LineShape(Vector2D(0,0),Vector2D(10,10)))
        })
        val inputRequest = InputRequest(8,None,vector2DGuide)
        Start('cad, "create.Input", inputRequest)
        //9 : Input type = KeyUp as input metod,
        //so the coordinates will be returned on key up
        //forward to the Input module with the shape guide.
      }


      //on first entry, send input request to input to get the start point for the move operations.
      case _ => {
        if (!Drawing.selection.isEmpty) {
          val vector2DGuide = Vector2DGuideKeys((v: Vector2D) => {
            Drawing.selection.transformation = origin
            val t : TransformationMatrix = TransformationMatrix(v, 1)
            // Return the shape, transformed
            Drawing.selection.transform(t).shapes.values
          })
          Start('cad, "create.Input", InputRequest(16,None,vector2DGuide))
        } else {
          Siigna display "Select objects to move"
          Start('cad, "Selection")
        }
      }
    }
  )
}