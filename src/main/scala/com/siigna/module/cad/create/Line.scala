/*
 * Copyright (c) 2008-2013, Selftie Software. Siigna is released under the
 * creative common license by-nc-sa. You are free
 *   to Share — to copy, distribute and transmit the work,
 *   to Remix — to adapt the work
 *
 * Under the following conditions:
 *   Attribution —   You must attribute the work to http://siigna.com in
 *                    the manner specified by the author or licensor (but
 *                    not in any way that suggests that they endorse you
 *                    or your use of the work).
 *   Noncommercial — You may not use this work for commercial purposes.
 *   Share Alike   — If you alter, transform, or build upon this work, you
 *                    may distribute the resulting work only under the
 *                    same or similar license to this one.
 *
 * Read more at http://siigna.com and https://github.com/siigna/main
 */

package com.siigna.module.cad.create

import com.siigna._
import app.Siigna

/**
 * A line module (draws one line-segment)
 */
class Line extends Module {


  val attributes = {
    val color = Siigna.color("activeColor")
    val lineWidth = Siigna.double("activeLineWidth")
    Attributes(Seq(color.map(c => "Color" -> color.getOrElse(None)), lineWidth.map(w => "StrokeWidth" -> lineWidth.getOrElse(None))).flatten)
  }

  var startPoint: Option[Vector2D] = None
  val stateMap: StateMap = Map(

    'Start -> {
      case End(MouseDown(p,MouseButtonRight,modifier)) :: tail => End
      case End(KeyDown(Key.escape,modifier)) :: tail => End

      case End(v : Vector2D) :: tail => {
        if (startPoint.isEmpty) {
          startPoint = Some(v)
          val vector2DGuide = Vector2DGuideNew((v : Vector2D) => {
            Array(LineShape(startPoint.get, v).addAttributes(attributes))
          })
          val inputRequest = InputRequestNew(7,startPoint,vector2DGuide)
          Start('cad,"create.InputNew", inputRequest)
        } else {
          val line = LineShape(startPoint.get,v).addAttributes(attributes)
          println("ATTR; "+attributes)
          Create(line)
          startPoint = None
          End
        }
      }

      //If input module returns nothing:
      case End("no point returned") :: tail => {
        if (startPoint.isEmpty) {
          Start('cad, "create.InputNew", InputRequestNew(6,None))
        } else {
          val vector2DGuide = Vector2DGuideNew((v : Vector2D) => {
            Array(LineShape(startPoint.get, v).addAttributes(attributes))
          })
          val inputRequest = InputRequestNew(7,startPoint,vector2DGuide)
          Start('cad,"create.InputNew", inputRequest)
      }}
      case End(k : KeyDown) :: tail => {
        // If the key is backspace without modification (shift etc), the last point is deleted, if there is any
        if (k == KeyDown(Key.Backspace,ModifierKeys(false,false,false))) {
          if (startPoint.isEmpty) {
            Start('cad, "create.InputNew", InputRequestNew(6,None))
          } else {
            val vector2DGuide = Vector2DGuideNew((v : Vector2D) => {
              Array(LineShape(startPoint.get, v).addAttributes(attributes))
            })
            val inputRequest = InputRequestNew(7,startPoint,vector2DGuide)
            Start('cad,"create.InputNew", inputRequest)
          }
        }
      }
      case _ => Start('cad, "create.InputNew", InputRequestNew(6,None))
    }
  )
}