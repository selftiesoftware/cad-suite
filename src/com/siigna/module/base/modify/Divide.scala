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

package com.siigna.module.base.modify

import com.siigna._
import com.siigna.module.base.Menu._

object Divide extends Module {

/**
 * A module to split a line into a number (specified by the user) of segments. Each segment has the length lineLength/segments.
 */

  def eventHandler = EventHandler(stateMap, stateMachine)

  val transformation = TransformationMatrix()
  var text         = ""
  var shape : Shape = LineShape(Vector2D(0,0),Vector2D(0,100))

  def stateMap     = DirectedGraph(
    'Start -> 'KeyDown -> 'End
  )

  lazy val stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      //start 'Divide only if there is a selection)

      if (Model.selection.isDefined) {
        shape = Model(Model.selection.get.parts.head._1)
        Siigna display "type number of subdivisions"
        Goto('TextInput)
      } else {
        Siigna display "Select line to divide"
        ForwardTo('Selection)
      }
    }),
    'TextInput -> ((events : List[Event]) => {
      events match {
        case KeyDown(Key.Backspace, _) :: tail => {
            if (text.length != 0) text = text.substring(0, text.length - 1)
            else Goto('End)
        }
        //create the division:
        case KeyDown(Key.Enter, _) :: tail => {
          var divisions = text.toInt
          shape match {
            case LineShape(p1, p2, _) => {

              var spacingX = (p2.x - p1.x)/divisions
              var spacingY = (p2.y - p1.y)/divisions

              //create the divisions  - one added to loop to prevent multiply by zero.
              for(i <- 1 to divisions +1 ) {
                var divisionX = p1.x + spacingX * i - spacingX
                var divisionY = p1.y + spacingY * i - spacingY
                var divisionPoint = Vector2D(divisionX, divisionY)
                var radius = divisionPoint + Vector2D(2,0)
                println(divisionPoint)
                Create(CircleShape(divisionPoint, radius))
              }
            }
            case PolylineShape (_ ,_ ,_) => {
              Siigna display "not implemented yet"
              Goto('End)
            }
            case ArcShape (_ , _, _, _, _) => {
              Siigna display "not implemented yet"
              Goto('End)
            }
            case CircleShape (_ ,_ ,_) => {
              Siigna display "not implemented yet"
              Goto('End)
            }

            case _=> Goto('End)
          }
          //Create(shapes.get.apply(transformation.get))
          text = ""
          Model.deselect()
          Goto('End)
        }
        case KeyDown(Key.Esc, _) :: tail => {
          text = ""
          Goto('End)
        }
        case KeyDown(key, _) :: tail => {
          text += key.toChar.toString.toLowerCase
          Siigna display text
        }
        case MouseMove(_, _, _) :: tail =>
        case MouseUp(_, MouseButtonRight, _) :: tail => Goto('End)
        case _ =>
      }
      None
    }),
    'End   -> ((events : List[Event]) => {
    //make the division here:
    })
  )
  override def paint(g : Graphics, t : TransformationMatrix) {

  }
}