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
import app.controller.Controller
import app.Siigna
import java.awt.Color

/**
 * A line module (draws one line-segment)
 */
class Line extends Module {

  var startPoint: Option[Vector2D] = None

  val stateMap: StateMap = Map(

    'Start -> {
      case End(v : Vector2D) :: tail => {
        if (startPoint.isEmpty){
          startPoint = Some(v)

          val guide = PointPointGuide(startPoint.get,(v : Vector2D) => {
            (Array(LineShape(startPoint.get, v)))
          },112)
          Start('Input,"com.siigna.module.base.create", guide)
        } else {

          val lShape = LineShape(startPoint.get,v)

          def setAttribute[T : Manifest](name:String, shape:Shape) = {
            Siigna.get(name) match {
              case s : Some[T] => shape.addAttribute(name, s.get)
              case None => shape// Option isn't set. Do nothing
            }
          }

          val line = setAttribute[Color]("Color",
            setAttribute[Double]("LineWeight", lShape)
          )
          Create(line)
          End
        }
      }

      //If input module returns nothing:
      case End("no point returned") :: tail => {
        if (startPoint.isEmpty) {
          Start('Input,"com.siigna.module.base.create",111)
        } else {
        val guide = PointPointGuide(startPoint.get, (v : Vector2D) => {
          (Array(LineShape(startPoint.get, v)))
        },112)//1 : Input type = InputTwoValues
        Start('Input,"com.siigna.module.base.create", guide)
      }}

      case End(k : KeyDown) :: tail => {
        // If the key is backspace without modification (shift etc), the last point is deleted, if there is any
        if (k == KeyDown(Key.Backspace,ModifierKeys(false,false,false))) {
          if (startPoint.isEmpty) {
            Start('Input,"com.siigna.module.base.create",111)
          } else {
            val guide = PointPointGuide(startPoint.get,(v : Vector2D) => {
              (Array(LineShape(startPoint.get, v)))
            },112)
            Start('Input,"com.siigna.module.base.create", guide)
      } } }
        
      case _ => Start('Input,"com.siigna.module.base.create",111)

    }
  )
}