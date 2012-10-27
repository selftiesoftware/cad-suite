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

/*package com.siigna.module.base.create

import com.siigna._
import module.base.Default

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
object Text extends Module {

  val eventHandler = new EventHandler(stateMap, stateMachine)

  var boundingRectangle : Option[Rectangle2D] = None
  var length = 1
  var text     = ""
  var position : Option[Vector2D] = None
  var rect : Option[Rectangle2D] = None
  var scale : Int  = 1
  var attributes = Attributes( "TextSize" -> 10)
  var shape : Option[TextShape] = None


//  var currentState = 0
//  var number   = 0

  def stateMap = DirectedGraph(

    //'StartCategory      -> 'KeyDown     -> 'TextInput,
    'StartCategory      -> 'KeyEscape   -> 'End,
    'TextInput  -> 'KeyEscape   -> 'End,
    'StartCategory      -> 'KeyEscape   -> 'End
  )

  def stateMachine = Map(
    'StartCategory -> ((events : List[Event]) => {
      Siigna.display("click to set text")
      events match {
        case Message(p : Vector2D) :: tail => {
          position = Some(p)
          Goto('TextInput)
        }
        case _ => Module('Point)
      }
      None
    }),
    'TextInput -> ((events : List[Event]) => {
      events match {
        case KeyDown(Key.Backspace, _) :: tail => {
            if (text.length != 0) text = text.substring(0, text.length - 1)
            else Goto('End)
        }
        case KeyDown(Key.Enter, _) :: tail => Goto('End)
        case KeyDown(Key.Esc, _) :: tail => {
          text = ""
          Goto('End)
        }
        case KeyDown(key, _) :: tail => {
          text += key.toChar.toString.toLowerCase
        }
        case MouseUp(_, MouseButtonRight, _) :: tail => Goto('End)
        case _ =>
      }
      None
    }),
    'End -> ((events : List[Event]) => {
      events match {
        case _ =>
          if (text.length > 0) {
            //move the text so that the lower left corner is located at the starting position
            var textPosition = Vector2D((position.get.x),(position.get.y+(Siigna.paperScale)))
            shape = Some(TextShape(text+" ", textPosition, scale * (Siigna.paperScale + 1), attributes))
            CreateCategory(shape)

            //clear the vars
            position = None
            text = ""
          }
          else None
      Default.previousModule = Some('Text)
      }
    })
  )
  override def paint(g : Graphics, t : TransformationMatrix) {
    //move the text so that the lower left corner is located at the starting position
    if (position.isDefined){
      var textPosition = Vector2D((position.get.x),(position.get.y+(Siigna.paperScale )))
      shape = Some(TextShape(text+" ", textPosition, (scale * (Siigna.paperScale + 1)), attributes))
      g draw shape.get.transform(t)
      length = text.length

      val y2Offset = 1*scale
      val posOffset = Vector2D(0,-1.2*scale)
      //draw a bounding rectangle guide for the text
      var x2 = Some(position.get.x+(length * 1.42 *scale))
      var y2 = Some(position.get.y+y2Offset+ scale)
      boundingRectangle = Some(Rectangle2D(position.get + posOffset,Vector2D(x2.get,y2.get)))
      g draw PolylineShape(boundingRectangle.get).setAttribute("Color" -> "#66CC66".color).transform(t)
    }
  }

//  override def paint(g : Graphics, t : TransformationMatrix) {
//    if (currentState == 1 ) {
//      // Draw a cursor.
//      val newShape = if (number <= 10) {
//        number += 1
//        TextShape(text+"|", position, scale)
//      } else if (number < 20) {
//        number += 1
//        TextShape(text+" ", position, scale)
//      } else {
//        number = 0
//        TextShape(text+" ", position, scale)
//      }
//
//      // Draw the shape in its current form.
//      g draw newShape.transform(t)
//    }
//  }
//
}
*/