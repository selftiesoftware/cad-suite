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

class Rotate extends Module {

  private var centerPoint : Option[Vector2D] = None
  private var endVector : Option[Vector2D] = None
  private var rotation : Double = 0.0
  private var startVector : Option[Vector2D] = None
  var transformation : Option[TransformationMatrix] = None

  val stateMap: StateMap = Map(

    'Start -> {
      case End(p : Vector2D) :: tail => {
        if(!centerPoint.isDefined) {
          centerPoint = Some(p)
          Siigna display "set or type rotation angle"
          Start('Point,"com.siigna.module.base.create")
        }

        // if the center of rotation and origin for rotation is set, return to Point to draw the rotation dynamically.
        else if(centerPoint.isDefined && !startVector.isDefined){
          startVector = Some(p)

          //a guide to get Point to dynamically draw the shape(s) and their rotation
          val shapeGuide = PointGuide(p, (v : Vector2D) => {
            // CreateCategory a matrix
            val t : TransformationMatrix = if (startVector.isDefined && centerPoint.isDefined) {
              // To find the angle between two vectors (lines): First calculate the separate angles
              val a1 : Double = (startVector.get - centerPoint.get).angle
              val a2 : Double = (v - centerPoint.get).angle
              // ... And then subtract the second from the first
              TransformationMatrix(Vector2D(0,0), 1).rotate(a2 - a1, centerPoint.get)
              // If no start- (or center-) point has been defined - create empty matrix
            } else TransformationMatrix()
            // Return the shape, transformed
            Drawing.selection.get.apply(t)
          },3) //1 : Input type = InputAngle
          Start('Point,"com.siigna.module.base.create", shapeGuide)
        //If a rotation-vector is set, do the rotation!
        } else if(startVector.isDefined && centerPoint.isDefined) {
           endVector = Some(p)

          val t : TransformationMatrix = {
            if (endVector.isDefined) {
              val a1 : Double = (startVector.get - centerPoint.get).angle
              val a2 : Double = (endVector.get - centerPoint.get).angle
              TransformationMatrix(Vector2D(0,0), 1).rotate(a2 - a1, centerPoint.get)
            } else TransformationMatrix()
          }
          //val t = transformation.get.rotate(rotation,centerPoint.get)
          Drawing.selection.get.transform(t)
          Drawing.deselect()
          End
        }
      }
      //if Point returns a typed angle:
      case End(a : Double) :: tail => {
        println("received a typed angle!")
      }

      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      case MouseDown(p, MouseButtonRight, _) :: tail => End
      case _ => {
        Siigna display "set base point for rotation"
        Start('Point,"com.siigna.module.base.create")
      }
    }
  )
}