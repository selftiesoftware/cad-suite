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

class Move extends Module {

  var endPoint : Option[Vector2D] = None

  //a guide to get Point to draw the shape(s) dynamically
  val shapeGuide : Vector2D => Traversable[Shape] = (v : Vector2D) => {
    // CreateCategory a matrix
    val t : TransformationMatrix = if (startPoint.isDefined) {
      TransformationMatrix(v - startPoint.get, 1)
    // If no startPoint has been defined - create an empty matrix
    } else TransformationMatrix()
    // Return the shape, transformed
    Drawing.selection.get.apply(t)
  }

  var startPoint : Option[Vector2D] = None
  var transformation : Option[TransformationMatrix] = None

  val stateMap: StateMap = Map(

    'Start -> {
      case End(p : Vector2D) :: tail => {
        if(!startPoint.isDefined) {
          startPoint = Some(p)
          Siigna display "set new location"
        } else {
          endPoint = Some(p)

          transformation = Some(TransformationMatrix((p - startPoint.get), 1))
          Drawing.selection.get.transform(transformation.get)
          Drawing.deselect()
        }
      }
      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      case MouseDown(p, MouseButtonRight, _) :: tail => End

      case _ => {
        Siigna display "set origin of move"
        Start('Point,"com.siigna.module.base.create")
      }

          //def getEndPoint(p : Vector2D) = {
          //  endPoint = Some(p)
          //  (p - startPoint.get)

          //}
          //if moving is initiated and completed by dragging the mouse:
          //if (startPoint.isDefined && moduleCallFromMenu == false) {
          //  val translation = events match {
          //    case MouseDown(p, _, _) :: tail => getEndPoint(p)
          //    case MouseDrag(p, _, _) :: tail => getEndPoint(p)
          //    case MouseMove(p, _, _) :: tail => getEndPoint(p)
          //    case MouseUp(p, _, _) :: tail => {
          //     ending = true
          //      getEndPoint(p)
          //    }


    })

  //draw the moving geometry when dragging the mouse
  override def paint(g : Graphics, t : TransformationMatrix) {
    Drawing.selection.foreach(s => transformation.foreach(s.apply(_).foreach(s => g.draw(s.transform(t)))))
  }

}