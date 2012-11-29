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

      //If the move module starts with a point, it knows where to start...
      case Start(_,p: Vector2D) :: tail => {
        //set the startpoint for the move operation (if not already set)
        startPoint = Some(p.transform(View.deviceTransformation))
        //definition of a shape guide that is used to send the selected shapes to the 'Point module
        // where they are drawn dynamically
        val shapeGuide = PointPointGuide(p, (v : Vector2D) => {
          val t : TransformationMatrix = if (startPoint.isDefined) {
            TransformationMatrix(v - startPoint.get, 1)
            // If no startPoint has been defined - create an empty matrix
          } else TransformationMatrix()
          // Return the shape, transformed
          Drawing.selection.get.apply(t)
        },9) //9 : Input type = KeyUp as input metod,
        //so the coordinates will be returned on key up
        //forward to the Input module with the shape guide.
        Start('Input,"com.siigna.module.base.create", shapeGuide)
      }

      case End(MouseUp(p,_,_)) :: tail => {
        //If start point is defined, it is where the move should end:
        if (!startPoint.isEmpty) {
          endPoint = Some(p)
          transformation = Some(TransformationMatrix((endPoint.get - startPoint.get), 1))
        //If start point is not defined, then p is the vector, that defines the move:
        } else {
          transformation = Some(TransformationMatrix(p, 1))
        }
        Drawing.selection.get.transform(transformation.get)
        Drawing.deselect()
        End
      }

      //If point returns mouse down, then this is where move starts, or ends, 
      // depending on whether there is a start point yet...
      case End(MouseDown(p,_,_)) :: tail => {
        if (startPoint.isEmpty) {
          startPoint = Some(p)
          val shapeGuide = PointPointGuide(p, (v : Vector2D) => {
            val t : TransformationMatrix = TransformationMatrix(v - startPoint.get, 1)
            // Return the shape, transformed
            Drawing.selection.get.apply(t)
          },112)
          Start('Input,"com.siigna.module.base.create", shapeGuide)
        } else {
          endPoint = Some(p.transform(View.deviceTransformation))
          transformation = Some(TransformationMatrix((endPoint.get - startPoint.get), 1))
          Drawing.selection.get.transform(transformation.get)
          Drawing.deselect()
          End
        }
      }  

      //If point returns point, then this is where the move should end...
      case End(p: Vector2D) :: tail => {
        if (!startPoint.isEmpty) {
          transformation = Some(TransformationMatrix((p - startPoint.get), 1))
          Drawing.selection.get.transform(transformation.get)
          Drawing.deselect()
          End
        }
      }

      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      case MouseDown(p, MouseButtonRight, _) :: tail => End
      case End :: tail => End

      //on first entry, go to point to get the start point for the move operations.
      case _ => {
        //Should be done differently, but this is how I can reach this (usableSelectionExists) function just quickly...
        val l = new ModuleInit
        if (l.usableSelectionExists) {
          //val p = Vector2D(0,0)
          val shapeGuide = PointGuide((v : Vector2D) => {
            val t : TransformationMatrix = TransformationMatrix(v, 1)
            // Return the shape, transformed
            Drawing.selection.get.apply(t)
          },1020)
          //Input type 1020: coordinates, mouse-drag-distance, or key-input (twoPoint or onePoint + guide),
          // do not draw guide in input until left mouse button is clicked (draw if dragging, do not draw if selecting start point)
          Start('Input, "com.siigna.module.base.create", shapeGuide)
        } else {
          Siigna display "nothing selected"
         End
        }
      }
    }
  )
}