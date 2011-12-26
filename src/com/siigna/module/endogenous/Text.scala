///* 2010 (C) Copyright by Siigna, all rights reserved. */
//
//package com.siigna.module.endogenous
//
//import java.awt.{Font, Color, TextField}
//import java.awt.font._
//import java.awt.event.{KeyEvent => AWTKeyEvent}
//
//import com.siigna.app.Siigna
//import com.siigna.app.model.shape._
//import com.siigna.app.view.Graphics
//import com.siigna.module.Module
//import com.siigna.util.action._
//import com.siigna.util.collection.DirectedGraph
//import com.siigna.util.event._
//import com.siigna.util.geom._
//
//object Text extends Module {
//
//  lazy val eventHandler = new EventHandler(stateMap, stateMachine)
//
//  def shape    = TextShape(text, position, scale)
//  var text     = ""
//  var position = Vector2D(0, 0)
//  var scale    = 10
//  var currentState = 0
//  var number   = 0
//
//  lazy val stateMap = DirectedGraph(
//    'Start      -> 'MouseUp     -> 'StartPoint,
//    'StartPoint -> 'KeyDown     -> 'TextInput,
//    'StartPoint -> 'KeyEscape   -> 'End,
//    'TextInput  -> 'KeyEscape   -> 'End,
//    'Start      -> 'KeyEscape   -> 'End
//  )
//
//  lazy val stateMachine = Map(
//    'Start -> ((events : List[Event]) => {
//      None
//    }),
//    'StartPoint -> ((events : List[Event]) => {
//      events match {
//        case MouseUp(p, MouseButtonLeft, _) :: tail => {
//          position = p
//          val physicalPosition = p.transform(Siigna.physical)
//          val field = new java.awt.TextField("")
//          interface.addComponent(field)
//          field.setLocation(physicalPosition.x.toInt, physicalPosition.y.toInt)
//          field.setColumns(10)
//          // TODO: Why isn't this working?!
//          println(Siigna.container.contains(physicalPosition.x.toInt, physicalPosition.y.toInt))
//          currentState = 1
//        }
//        case MouseUp(_, MouseButtonRight, _) :: tail => {
//          goto('End)
//        }
//        case _ =>
//      }
//      None
//    }),
//    'TextInput -> ((events : List[Event]) => {
//      events match {
//        case KeyDown(Key.Backspace, _) :: tail => {
//            if (text.length != 0) text = text.substring(0, text.length - 1)
//            else goto('End)
//        }
//        case KeyDown(Key.Enter, _) :: tail => goto('End)
//        case KeyDown(char, _) :: tail => text += char.toString.toLowerCase
//        case _ =>
//      }
//      None
//    }),
//    'End -> ((events : List[Event]) => {
//      if (text.length > 0) Some(CreateShape(shape))
//      else None
//    })
//  )
//
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
//}
