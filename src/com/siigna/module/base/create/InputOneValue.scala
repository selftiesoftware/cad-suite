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

  var relativeX : Double = 0.0

  var doubleGuide : Option[Double => Traversable[Shape]] = None
  var pointGuide : Option[Vector2D => Traversable[Shape]] = None
  var startPoint : Option[Vector2D] = None
  var inputType: Option[Int] = None

  val stateMap: StateMap = Map(

    'Start -> {

      case Start(_ ,g: DoubleGuide) :: KeyDown(code, _) :: tail => {
        //Ends if enter was pressed...
        if (code == Key.enter) {
          End(0.0)
        //Othervise it starts:
        } else if (code.toChar.isDigit || code.toChar.toString == "-" || code.toChar.toString == ".") {
          coordinateValue += code.toChar
          doubleGuide = Some(g.doubleGuide)
          inputType = Some(g.inputType)

          Siigna display coordinateValue
        }
      }

      case Start(_ ,g: PointDoubleGuide) :: KeyDown(code, _) :: tail => {
        doubleGuide = Some(g.doubleGuide)
        inputType = Some(g.inputType)
        startPoint = Some(g.point1)
        //save the already typed key:
        if (code.toChar.isDigit) coordinateValue += code.toChar
        if (code.toChar.toString == "-" && coordinateValue.length() == 0) coordinateValue += code.toChar
        if (code.toChar.toString == "." && coordinateValue.length() == 0) coordinateValue += code.toChar
        Siigna display coordinateValue
      }

      case Start(_ ,g: PointPointDoubleGuide) :: KeyDown(code, _) :: tail => {
        doubleGuide = Some(g.doubleGuide)
        inputType = Some(g.inputType)
        //save the already typed key:
        if (code.toChar.isDigit) coordinateValue += code.toChar
        if (code.toChar.toString == "-" && coordinateValue.length() == 0) coordinateValue += code.toChar
        if (code.toChar.toString == "." && coordinateValue.length() == 0) coordinateValue += code.toChar
        Siigna display coordinateValue
      }

      case Start(_ ,g: PointGuide) :: KeyDown(code, _) :: tail => {
        pointGuide = Some(g.pointGuide)
        inputType = Some(g.inputType)
        //save the already typed key:
        if (code.toChar.isDigit) coordinateValue += code.toChar
        if (code.toChar.toString == "-" && coordinateValue.length() == 0) coordinateValue += code.toChar
        if (code.toChar.toString == "." && coordinateValue.length() == 0) coordinateValue += code.toChar
        Siigna display coordinateValue
      }

      case Start(_ ,g: PointPointGuide) :: KeyDown(code, _) :: tail => {
        startPoint = Some(g.point1)
        pointGuide = Some(g.pointGuide)
        inputType = Some(g.inputType)
        //save the already typed key:
        if (code.toChar.isDigit) coordinateValue += code.toChar
        if (code.toChar.toString == "-" && coordinateValue.length() == 0) coordinateValue += code.toChar
        if (code.toChar.toString == "." && coordinateValue.length() == 0) coordinateValue += code.toChar
        Siigna display coordinateValue
      }


      //Read numbers and minus, "," and enter as first entry if no guide is provided:
      case Start(_,_) :: KeyDown(code, _) :: tail => {
        //save the already typed key:
        if (code.toChar.isDigit) coordinateValue += code.toChar
        if (code.toChar.toString == "-" && coordinateValue.length() == 0) coordinateValue += code.toChar
        if (code.toChar.toString == "." && coordinateValue.length() == 0) coordinateValue += code.toChar

        Siigna display coordinateValue

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
        //The string must be at least a space - an empty string makes the message-function puke...
        //The string must be at least a space - an empty string makes the message-function puke...
        if (coordinateValue.length == 0) coordinateValue += " "
        Siigna display coordinateValue
      }

      //if point returns a keyDown - that is not previously intercepted
      case KeyDown(code, _) :: tail => {
        //get the input from the keyboard if it is numbers, (-) or (.)
        val char = code.toChar
        if (char.isDigit)
          coordinateValue += char
        else if ((char == '.') && !coordinateValue.contains('.')) {
           if (coordinateValue == " ") coordinateValue = "."
           else coordinateValue += "."
        }
        else if (char == '-' && coordinateValue.length < 1)
          coordinateValue = "-"
        else if (char == '-' && coordinateValue == " ")
          coordinateValue = "-"

        //Display the input in a message
        Siigna display coordinateValue
      }
      case _ => {
      }
    })

  override def paint(g : Graphics, t: TransformationMatrix) {
    //if points are in the process of being typed, then draw the shape dynamically on the basis of what coords are given.

    if(doubleGuide.isDefined && coordinateValue.length > 0 && coordinateValue != " " && coordinateValue != "-" && coordinateValue != "." && coordinateValue != "-."){
      val x = java.lang.Double.parseDouble(coordinateValue)
      if (x != 0) doubleGuide.foreach(_(x).foreach(s => g.draw(s.transform(t))))
    }
    if((inputType == Some(111) || inputType == Some(112)) && pointGuide.isDefined && coordinateValue.length > 0 && coordinateValue != " " && coordinateValue != "-" && coordinateValue != "." && coordinateValue != "-."){
      val x = java.lang.Double.parseDouble(coordinateValue)
      if (x != 0) pointGuide.foreach(_(Track.getPointFromDistance(x).get).foreach(s => g.draw(s.transform(t))))
    }
  }
}