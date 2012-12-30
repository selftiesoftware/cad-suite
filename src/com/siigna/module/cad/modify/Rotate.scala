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

package com.siigna.module.cad.modify

import com.siigna._
import app.Siigna
import com.siigna.module.cad.create._
import com.siigna.module.ModuleInit

class Rotate extends Module {

  private var centerPoint : Option[Vector2D] = None
  private var endVector : Option[Vector2D] = None
  var startVectorSet: Boolean = false

  private var startVector : Option[Vector2D] = Some(Vector2D(0,0))
  var transformation : Option[TransformationMatrix] = None

  val stateMap: StateMap = Map(

    'Start -> {
      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      case MouseDown(p, MouseButtonRight, _) :: tail => End
      case End(KeyDown(Key.Esc, _)) :: tail => End
      case End(MouseDown(p, MouseButtonRight, _)) :: tail => End

      case End(p : Vector2D) :: tail => {
        if(!centerPoint.isDefined) {
          centerPoint = Some(p)
          Siigna display "click to set rotation start point, or type a rotation angle"
          val doubleGuide = DoubleGuide((a : Double) => {
            val t : TransformationMatrix =
              TransformationMatrix( ).rotate(-a, centerPoint.get)
            Drawing.selection.get.apply(t)
          })

          val inputRequest = InputRequest(None,Some(doubleGuide),None,None,None,None,centerPoint,None,None,Some(120))
          Start('cad, "create.Input", inputRequest)
        }

        // if the center of rotation and origin for rotation is set, return to Point to draw the rotation dynamically.
        else if(centerPoint.isDefined && startVectorSet == false){
          startVector = Some(p)
          startVectorSet = true
          Siigna display "click to finish rotation, or type a rotation angle"
          val doubleGuide = DoubleGuide((d: Double) => {
            val t : TransformationMatrix = TransformationMatrix( ).rotate(-d, centerPoint.get)
            Drawing.selection.get.apply(t)
          })
          val vector2DGuide = Vector2DGuide((v: Vector2D) => {
            //Angle of line from reference point to mouse position:
            val d : Double = (-((mousePosition.transform(View.deviceTransformation) - centerPoint.get).angle - (startVector.get - centerPoint.get).angle))
            val t : TransformationMatrix = TransformationMatrix( ).rotate(-d, centerPoint.get)
            Drawing.selection.get.apply(t)
          })
          val inputRequest = InputRequest(Some(vector2DGuide),Some(doubleGuide),None,None,None,None,None,None,None,Some(13))
          //13 : Input type = Double from keys, or Vector2D from mouseDown.
          Start('cad, "create.Input", inputRequest)
          //If a rotation-vector is set, do the rotation!
        } else if(startVectorSet == true && centerPoint.isDefined) {
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
        println("GOT TYPED ANGLE: "+a)
        val t : TransformationMatrix = {
          if (centerPoint.isDefined) {
            TransformationMatrix(Vector2D(0,0), 1).rotate(-a, centerPoint.get)
          } else TransformationMatrix()
        }
        //val t = transformation.get.rotate(rotation,centerPoint.get)
        Drawing.selection.get.transform(t)
        Drawing.deselect()
        End
      }


      case _ => {
        //Should be done differently, but this is how I can reach this (usableSelectionExists) function just quickly...
        val l = new ModuleInit
        if (l.usableSelectionExists) {
          Siigna display "set centre point for rotation"
          Start('cad, "create.Input", 1)
        } else {
          Siigna display "nothing selected"
          End
        }
      }
    }
  )
}