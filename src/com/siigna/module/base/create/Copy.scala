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

package com.siigna.module.base.create

import com.siigna._
import app.Siigna
import com.siigna.module.base.create._
import module.ModuleInit

class Copy extends Module {

  var endPoint : Option[Vector2D] = None
  var multiCopy = false
  var startPoint : Option[Vector2D] = None
  var transformation : Option[TransformationMatrix] = None
  var shapes : Option[Selection] = None

  val stateMap: StateMap = Map(
    'Start -> {

      //If the move module starts with a point, it knows where to start...
      case Start(_,p: Vector2D) :: tail => {
        if(!Drawing.selection.isEmpty) {
          //set the startpoint for the move operation (if not already set)
          startPoint = Some(p.transform(View.deviceTransformation))

          shapes = Some(Drawing.selection.get)

          //definition of a shape guide that is used to send the selected shapes to the 'Point module
          // where they are drawn dynamically
          val shapeGuide = PointPointGuide(p, (v : Vector2D) => {
            val t : TransformationMatrix = if (startPoint.isDefined) {
              TransformationMatrix(v - startPoint.get, 1)
              // If no startPoint has been defined - create an empty matrix
            } else TransformationMatrix()
            // Return the shape, transformed
            Drawing.selection.get.apply(t)
          },9) //9 : Input type = KeyUp as input method,
          //so the coordinates will be returned on key up
          //forward to the Input module with the shape guide.
          Start('Input,"com.siigna.module.base.create", shapeGuide)
        }
      }
      case End(MouseUp(p,_,_)) :: tail => {
        //If start point is defined, it is where the copy should end:
        if (startPoint.isDefined) {
          endPoint = Some(p)
          multiCopy = true
          transformation = Some(TransformationMatrix((endPoint.get - startPoint.get), 1))

          Siigna display "optional: type number of copies"
          Start('Point, "com.siigna.module.base.create",10)
        } else {
          transformation = Some(TransformationMatrix(p, 1))
        }
      }

      //If point returns mouse down, then this is where move starts, or ends,
      // depending on whether there is a start point yet...
      case End(MouseDown(p,_,_)) :: tail => {
        if (!startPoint.isDefined && !Drawing.selection.isEmpty) {
          startPoint = Some(p)
          val shapeGuide = PointPointGuide(p, (v : Vector2D) => {
            val t : TransformationMatrix = TransformationMatrix(v - startPoint.get, 1)
            // Return the shape, transformed
            Drawing.selection.get.apply(t)
          },1)
          Start('Input,"com.siigna.module.base.create", shapeGuide)
        }
      }

      //If point returns point, then this is where the move should end...
      case End(p: Vector2D) :: tail => {
        if (!startPoint.isEmpty) {
          transformation = Some(TransformationMatrix((p - startPoint.get), 1))
        }
      }
      //multicopy
      case End(f : Double) :: tail => {
        if (multicopy == true && endPoint.isDefined && multicopy == true){
          for (i <- 0 to f.toInt) {
            Create(shapes.get.apply(TransformationMatrix(Vector2D((endPoint.get.x - startPoint.get.x) * i, (endPoint.get.y - startPoint.get.y) * i), 1)))
            End
          }
        } else End
      }
      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      case MouseDown(p, MouseButtonRight, _) :: tail => End
      case End :: tail => End

      //on first entry, go to point to get the start point for the move operations.
      case _ => {

        Siigna display "click to set origin"

        //Should be done differently, but this is how I can reach this (usableSelectionExists) function just quickly...
        val l = new ModuleInit
        if (l.usableSelectionExists) {
          //val p = Vector2D(0,0)
          val shapeGuide = PointGuide((v : Vector2D) => {
            val t : TransformationMatrix = TransformationMatrix(v, 1)
            // Return the shape, transformed
            Drawing.selection.get.apply(t)
          },102) //Input type 102: coordinates, mouse-drag-distance, or key-input.
          Start('Input, "com.siigna.module.base.create", shapeGuide)
        } else {
          Siigna display "nothing selected"
          End
        }
      }
    }
  )
}