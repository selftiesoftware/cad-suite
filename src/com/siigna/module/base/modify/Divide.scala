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

/**
 * A module to split a line into a number (specified by the user) of segments. Each segment has the length lineLength/segments.
 */
class Divide extends Module {

  val transformation = TransformationMatrix()
  var text         = ""
  var shape : Shape = LineShape(Vector2D(0,0),Vector2D(0,100))

  lazy val stateMap : StateMap = Map(
    'Start -> {
      case _ => {
        //start 'Divide only if there is a selection)

        if (Drawing.selection.isDefined) {
          shape = Drawing(Drawing.selection.get.parts.head._1)
          Siigna display "type number of subdivisions"
          'TextInput
        } else {
          Siigna display "Select line to divide"
          Module('Selection)
        }
      }
    },
    'TextInput -> {
      case KeyDown(Key.Backspace, _) :: tail => {
        if (text.length != 0) text = text.substring(0, text.length - 1)
        else 'End
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
              Create(CircleShape(divisionPoint, radius))
            }
          }
          case PolylineShape (_ ,_ ,_) => {
            Siigna display "not implemented yet"
            'End
          }
          case ArcShape (_ , _, _, _, _) => {
            Siigna display "not implemented yet"
            'End
          }
          case CircleShape (_ ,_ ,_) => {
            Siigna display "not implemented yet"
            'End
          }

          case _=> 'End
        }
        //CreateCategory(shapes.get.apply(transformation.get))
        text = ""
        Drawing.deselect()
        'End
      }
      case KeyDown(Key.Esc, _) :: tail => {
        text = ""
        'End
      }
      case KeyDown(key, _) :: tail => {
        text += key.toChar.toString.toLowerCase
        Siigna display text
      }
      case MouseMove(_, _, _) :: tail =>
      case MouseUp(_, MouseButtonRight, _) :: tail => 'End
      case _ =>
    },
    'End -> { case _ => } //make the division here:
  )
  override def paint(g : Graphics, t : TransformationMatrix) {

  }
}