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

package com.siigna.module.cad.helpers

import com.siigna._
import com.siigna.module.cad.create._
import app.Siigna
import module.Tooltip

/**
 * A line module (draws one line-segment)
 */
class Distance extends Module {

  // var guide : Guide = Guide((v : Vector2D) => {
  //   Array(LineShape(startPoint.get, v))
  // })

  var startPoint: Option[Vector2D] = None

  def stateMap: StateMap = Map(

    'Start -> {
      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      case MouseDown(p, MouseButtonRight, _) :: tail => End
      case End(KeyDown(Key.Esc, _)) :: tail => End
      case End(MouseDown(p, MouseButtonRight, _)) :: tail => End

      case End(v : Vector2D) :: tail => {
        if (!startPoint.isDefined){
          startPoint = Some(v)
          val vector2DGuide = Vector2DGuide((p: Vector2D) => Traversable(LineShape(startPoint.get, p)))
          Start('cad, "create.Input", InputRequest(6,startPoint,vector2DGuide))

        } else if (startPoint.isDefined) {
          var length = ((startPoint.get - v).length).round
          Siigna display "length: " + length
          Tooltip.blockUpdate(3500)
          End
        }
      }
      case _ => {
        Tooltip.updateTooltip("Distance measure tool active")
        Siigna display "set two points to measure the distance between them."
        Tooltip.blockUpdate(3500)
        Start('cad, "create.Input", InputRequest(6,None))
      }
      //if

    }
  )
}