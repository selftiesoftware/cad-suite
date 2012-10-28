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

class Line extends Module{

  var startPoint: Option[Vector2D] = None

  val stateMap: StateMap = Map(

    'Start -> {
      case ModuleEnd(v: Vector2D)::tail => {
        println(startPoint)
        if (startPoint.isEmpty){
          startPoint = Some(v)
          println(startPoint)
          Module('Point,"com.siigna.module.base.create")
        } else {

          val lShape = LineShape(startPoint.get,v)

          def setAttribute[T](name:String, shape:Shape) = {
            Siigna.get(name) match {
              case Some(t: T) => shape.addAttribute(name, t)
              case None => shape// Option isn't set. Do nothing
            }
          }


          val line = setAttribute[Color]("Color",
            setAttribute[Double]("LineWeight", lShape)
          )

          Create(line)
          ModuleEnd
        }
      }
      case _ => Module('Point,"com.siigna.module.base.create")
    }
  )
}