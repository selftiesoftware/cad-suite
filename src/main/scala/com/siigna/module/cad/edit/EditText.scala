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

package com.siigna.module.cad.edit

import com.siigna._
import module.cad.create.{InputRequest, DynamicDrawFromText}
import module.Tooltip

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
class EditText extends Module {

  var text     = ""
  var position : Option[Vector2D] = None
  var scale : Int  = 5
  var attributes = Attributes( "TextSize" -> 10)
  var shape : Option[TextShape] = None
  var oldShape: Option[TextShape] = None
  var oldShapeId: Option[Int] = None


//  var currentState = 0
//  var number   = 0

  val stateMap: StateMap = Map(

    'Start -> {
      case End(MouseDown(p,MouseButtonRight,modifier)) :: tail => End
      case End(KeyDown(Key.escape,modifier)) :: tail => End

      case End(s : String) :: tail => {
        if (s.length > 0) {
          //move the text so that the lower left corner is located at the starting position
          val textPosition = position.get
          shape = Some(TextShape(s + " ", textPosition, scale * (Siigna.paperScale + 1), oldShape.get.attributes))
          Create(shape.get)
          End
        }
      }

      case _ => {
        Tooltip.updateTooltip(List("Edit text tool active"))
        var n : Int = 0
        Drawing.selection.shapes.foreach(m => {
          m._2 match {
            case t: TextShape => {
              println("TEXT SHAPE i selection")
              n = n + 1
              oldShape = Some(t)
              oldShapeId = Some(m._1)
            }
            case _ => println("ingen textshape i selection")
          }
        })
        if (n == 1) {
          Siigna.display("Edit text")
          Tooltip.blockUpdate(3500)
          position = Some(oldShape.get.position)
          val textGuide: DynamicDrawFromText = DynamicDrawFromText((s: String) => Traversable(TextShape(s + " ", position.get,  scale * (Siigna.paperScale + 1))))
          val inputRequest = InputRequest(17,None,textGuide)
          Start('cad,"create.Input", inputRequest)
        } else {
          Siigna.display("Excatly one text-shape must be selected to edit text.")
          Tooltip.blockUpdate(3500)
          End
        }
      }
    }
  )
}
