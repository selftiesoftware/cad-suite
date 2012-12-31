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

package com.siigna.module.cad.create

import com.siigna._
import app.Siigna
import java.awt.Color

/**
 * A line module (draws one line-segment)
 */
class Line extends Module {

  var color = Siigna("activeColor")
  val stroke = Siigna("activeLineWidth")
  var startPoint: Option[Vector2D] = None
  val stateMap: StateMap = Map(

    'Start -> {
      case End(MouseDown(p,MouseButtonRight,modifier)) :: tail => End
      case End(KeyDown(Key.escape,modifier)) :: tail => End

      case End(v : Vector2D) :: tail => {
        if (startPoint.isEmpty) {
          startPoint = Some(v)
          val vector2DGuide = Vector2DGuide((v : Vector2D) => {
            Array(LineShape(startPoint.get, v).addAttributes("Color" -> color , "StrokeWidth" -> stroke))
          })
          val inputRequest = InputRequest(Some(vector2DGuide),None,None,None,None,None,startPoint,None,None,Some(112))
          Start('cad,"create.Input", inputRequest)
        } else {
          val line = LineShape(startPoint.get,v).addAttributes("Color" -> color , "StrokeWidth" -> stroke)
          Create(line)
          startPoint = None
          End
        }
      }

      //If input module returns nothing:
      case End("no point returned") :: tail => {
        if (startPoint.isEmpty) {
          Start('cad, "create.Input", 111)
        } else {
          val vector2DGuide = Vector2DGuide((v : Vector2D) => {
            Array(LineShape(startPoint.get, v).addAttributes("Color" -> color , "StrokeWidth" -> stroke))
          })
          val inputRequest = InputRequest(Some(vector2DGuide),None,None,None,None,None,startPoint,None,None,Some(112))
          Start('cad,"create.Input", inputRequest)
      }}
      case End(k : KeyDown) :: tail => {
        // If the key is backspace without modification (shift etc), the last point is deleted, if there is any
        if (k == KeyDown(Key.Backspace,ModifierKeys(false,false,false))) {
          if (startPoint.isEmpty) {
            Start('cad, "create.Input", 111)
          } else {
            val vector2DGuide = Vector2DGuide((v : Vector2D) => {
              Array(LineShape(startPoint.get, v).addAttributes("Color" -> color , "StrokeWidth" -> stroke))
            })
            val inputRequest = InputRequest(Some(vector2DGuide),None,None,None,None,None,startPoint,None,None,Some(112))
            Start('cad,"create.Input", inputRequest)
          }
        }
      }
      case _ => Start('cad, "create.Input", 111)
    }
  )
}