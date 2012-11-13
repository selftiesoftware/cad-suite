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

package com.siigna.module.base.helpers

import com.siigna._
import com.siigna.module.base.create._
import app.Siigna

/**
 * A line module (draws one line-segment)
 */
class Distance extends Module {

  // var guide : Guide = Guide((v : Vector2D) => {
  //   Array(LineShape(startPoint.get, v))
  // })

  var startPoint: Option[Vector2D] = None

  val stateMap: StateMap = Map(

    'Start -> {
      case End(v : Vector2D) :: tail => {
        if (startPoint.isEmpty){
          startPoint = Some(v)

          val guide = PointPointGuide(v, (v : Vector2D) => {
            (Array(LineShape(startPoint.get, v)))
          },1)//1 : Input type = InputTwoValues

          Start('Point,"com.siigna.module.base.create", guide)
        } else {
          var length : Int = ((startPoint.get - v).length).toInt
          Siigna display "length: " + length
          End
        }
      }
      case _ => {
        Siigna display "set two points to measure the distance between them."
        Start('Point,"com.siigna.module.base.create")
      }
      //if

    }
  )
}