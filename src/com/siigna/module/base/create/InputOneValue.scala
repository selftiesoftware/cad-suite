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
import app.Siigna

/**
 * A module that collect a pair of digits on the basis of key inputs.
 * Used by modules that need eg. an X and Y coordinate to define a point.
 */

class InputOneValue extends Module {

  private var coordinateValue : String = ""  //input string for distances

  var pointGuide : Option[Vector2D => Traversable[Shape]] = None
  var startPoint : Option[Vector2D] = None

  val stateMap: StateMap = Map(

    'Start -> {

      case Start(_ ,g: PointPointGuide) :: KeyDown(code, _) :: tail => {
        pointGuide = Some(g.pointGuide)
        startPoint = Some(g.point)
      }

      //Ends on return, komma, TAB - returning value:
      case KeyDown(Key.Enter | Key.Tab | (','), _) :: tail => {
        if (coordinateValue.length > 0) {
            var value = Some(java.lang.Double.parseDouble(coordinateValue))
            End(value.get)
          } else End(0.0)
        }

      case KeyDown(Key.Backspace, _) :: tail => {
        if (coordinateValue.length > 0) coordinateValue = coordinateValue.substring(0, coordinateValue.length-1)
        Siigna display coordinateValue
      }

      //if point returns a keyDown - that is not previously intercepted
      case KeyDown(code, _) :: tail => {
        //get the input from the keyboard if it is numbers, (-) or (.)
        val char = code.toChar
        if (char.isDigit)
          coordinateValue += char
        else if ((char == '.') && !coordinateValue.contains('.'))
          coordinateValue += "."
        else if (char == '-' && coordinateValue.length < 1)
          coordinateValue = "-"

        //Display the input in a message
        Siigna display coordinateValue
      }
      case _ => {
      }
    })
}