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
        },3) //3 : Input type = InputTwoValues, and accepts KeyUp as input metod
        //forward to the point module with the shape guide.
        Start('Point,"com.siigna.module.base.create", shapeGuide)
      }

      case End(MouseUp(p,_,_)) :: tail => {
        endPoint = Some(p.transform(View.deviceTransformation))
        transformation = Some(TransformationMatrix((endPoint.get - startPoint.get), 1))
        Drawing.selection.get.transform(transformation.get)
        Drawing.deselect()
        End
      }

      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      case MouseDown(p, MouseButtonRight, _) :: tail => End

      //on first entry, go to point to get the start point for the move operations.
      case _ => {
        Siigna display "set origin of move"
        Start('Point,"com.siigna.module.base.create")
      }
    }
  )
}