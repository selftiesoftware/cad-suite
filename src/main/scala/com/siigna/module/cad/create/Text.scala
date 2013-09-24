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
import module.{Tooltip, ModuleInit}

//import java.awt.{Font, Color, TextField}
//import java.awt.font._
//import java.awt.event.{KeyEvent => AWTKeyEvent}

//import com.siigna.app.Siigna
//import com.siigna.app.model.shape._
//import com.siigna.app.view.Graphics
//import com.siigna.module.Module
//import com.siigna.util.action._
//import com.siigna.util.collection.DirectedGraph
//import com.siigna.util.event._
//import com.siigna.util.geom._
//
class Text extends Module {

  var text     = ""
  var position : Option[Vector2D] = None
  var scale : Int  = 5
  var attributes = Attributes( "TextSize" -> 10)
  var shape : Option[TextShape] = None


//  var currentState = 0
//  var number   = 0

  val stateMap: StateMap = Map(

    'Start -> {
      case End(MouseDown(p,MouseButtonRight,modifier)) :: tail => End
      case End(KeyDown(Key.escape,modifier)) :: tail => End
      case End(p : Vector2D) :: tail => {
        position = Some(p)
        val textGuide: TextGuide = TextGuide((s: String) => Traversable(TextShape(s + " ", p,  scale * (Siigna.paperScale + 1))))
        //val inputRequest = InputRequest(None,None,Some(textGuide),None,None,None,position,None,None,Some(14))
        //Start('cad,"create.Input", inputRequest)
        val inputRequest = InputRequest(12,None,textGuide)
        Siigna.display("type text")
        Start('cad,"create.Input", inputRequest)
      }

      case End(s : String) :: tail => {
        if (s.length > 0) {
          //move the text so that the lower left corner is located at the starting position
          val textPosition = position.get
          shape = Some(TextShape(s + " ", textPosition, scale * (Siigna.paperScale + 1), attributes))
          Create(shape.get)
          End
        }
      }

      case _ => {
        Tooltip.updateTooltip("Text tool active")
        Siigna.display("click to set text")
        Start('cad, "create.Input", InputRequest(6,None))
      }
    }
  )
}
