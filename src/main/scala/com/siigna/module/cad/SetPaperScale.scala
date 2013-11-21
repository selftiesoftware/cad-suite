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

package com.siigna.module.cad

import com.siigna._
import module.{Tooltip, ModuleInit}
import com.siigna.module.cad.create.{InputRequest, DynamicDrawFromText}

class SetPaperScale extends Module{


  var text     = ""
  var position : Option[Vector2D] = None
  var scale : Double  = 2.5
  var attributes = Attributes( "TextSize" -> 10)
  var shape : Option[TextShape] = None

  //  var currentState = 0
  //  var number   = 0

  val stateMap: StateMap = Map(

    'Start -> {

      case End(s : String) :: tail => {
        if (s.length > 0) {
          try {
            val scale = s.toInt
            //CHANGE SCALE HERE
            Siigna display("setting new scale: 1:"+scale)
            End
          } catch {
            case e : Exception  => {
              Siigna display("not a valid scale")
              End
            }
          }
        }
      }

      case e => {
        println("event; "+e)
        val p = Drawing.boundary.bottomRight
        val textGuide: DynamicDrawFromText = DynamicDrawFromText((s: String) => Traversable(TextShape("1:" + s + " ", p,  10)))
        //val inputRequest = InputRequest(None,None,Some(textGuide),None,None,None,position,None,None,Some(14))
        //Start('cad,"create.Input", inputRequest)
        val inputRequest = InputRequest(12,None,textGuide)
        Siigna.display("type the scale factor by which the drawing is reduced")
        Tooltip.blockUpdate(3500)
        Start('cad,"create.Input", inputRequest)
      }
    }
  )
}
