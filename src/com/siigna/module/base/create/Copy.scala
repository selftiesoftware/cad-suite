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

class Copy extends Module {

  var endPoint : Option[Vector2D] = None
  var startPoint : Option[Vector2D] = None
  var shapes : Option[Selection] = None
  var transformation : Option[TransformationMatrix] = None

  val stateMap: StateMap = Map(

    'Start -> {
      case End(p : Vector2D) :: tail => {
        println("START WITH POINT")
        if(!startPoint.isDefined && !Drawing.selection.get.isEmpty) {
          shapes = Some(Drawing.selection.get)
          startPoint = Some(p)
          Siigna display "set destination"

          val shapeGuide = PointGuide(p, (v : Vector2D) => {
            val t : TransformationMatrix = if (startPoint.isDefined) {
              TransformationMatrix(v - startPoint.get, 1)
              // If no startPoint has been defined - create an empty matrix
            } else TransformationMatrix()
            // Return the shape, transformed
            Drawing.selection.get.apply(t)
          })
          Start('Point,"com.siigna.module.base.create", shapeGuide)
        }
        else if(startPoint.isDefined){
          endPoint = Some(p)
          transformation = Some(TransformationMatrix((p - startPoint.get), 1))
          Create(shapes.get.apply(transformation.get))

          //TODO: implement multicopy
          //Siigna display "type number of copies"
          End
        } else Siigna display "select object(s) to copy"
      }
      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      case MouseDown(p, MouseButtonRight, _) :: tail => End

      case _ => {
        println("START WITHOUT HPOINT")
        Siigna display "set origin of copy"
        Start('Point,"com.siigna.module.base.create")
      }
    }
  )
}