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

/* 2012 (C) Copyright by Siigna, all rights reserved. */

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
  var scale    = 6
  var attributes = Attributes( "TextSize" -> 10)
  var shape : Option[TextShape] = None

//  var currentState = 0
//  var number   = 0

  def stateMap = DirectedGraph(
    'Start      -> 'MouseUp     -> 'StartPoint,
    'StartPoint -> 'KeyDown     -> 'TextInput,
    'StartPoint -> 'KeyEscape   -> 'End,
    'TextInput  -> 'KeyEscape   -> 'End,
    'Start      -> 'KeyEscape   -> 'End
  )

  def stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      None
    }),
    'StartPoint -> ((events : List[Event]) => {
      events match {
        case MouseDown(p, _, _) :: tail => {
          position = Some(p)
        }
        case MouseUp(_, _, _) :: tail => {
          if (position.isDefined){

            Goto('TextInput)
          }
        }
        //}
        case _ =>
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
            var textPosition = Vector2D((position.get.x),(position.get.y+(scale+scale/4)))
            shape = Some(TextShape(text+" ", textPosition, scale, attributes))
            Create(shape)

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
    var textPosition = Vector2D((position.get.x),(position.get.y+(scale+scale/4)))
    shape = Some(TextShape(text+" ", textPosition, scale, attributes))
    g draw shape.get.transform(t)
    if (position.isDefined){
      length = text.length
        //draw a bounding rectangle guide for the text
        var x2 = Some(position.get.x+(4+length*5.3))
        var y2 = Some(position.get.y+(scale+scale/2))
        boundingRectangle = Some(Rectangle2D(position.get,Vector2D(x2.get,y2.get)))
      g draw PolylineShape.fromRectangle(boundingRectangle.get).addAttribute("Color" -> "#66CC66".color).transform(t)
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
