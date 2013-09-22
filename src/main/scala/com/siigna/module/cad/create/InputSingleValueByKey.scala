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
import app.Siigna

/**
 * A module that collect a pair of digits on the basis of key inputs.
 * Used by modules that need eg. an X and Y coordinate to define a point.
 */

class InputSingleValueByKey extends Module {

  private var coordinateValue : String = ""  //input string for distances

  var relativeX : Double = 0.0

  //Information received from calling module
  var inputRequest: Option[InputRequest] = None
  var inputType: Option[Int] = None
  var guides: Seq[Guide] = Seq()
  var referencePoint: Option[Vector2D] = None

  var startPoint : Option[Vector2D] = None

  val stateMap: StateMap = Map(

    'Start -> {

      //exit mechanisms
      case MouseDown(p,MouseButtonRight,modifier) :: tail => End
      case KeyDown(Key.escape,modifier) :: tail => End

      case MouseDown(p,MouseButtonRight,modifier) :: tail => End(MouseDown(p,MouseButtonRight,modifier))

      //If left mouse is clicked, the module ends - if there is useful double input, it is returned, if not, the module just ends.
      case MouseDown(p,MouseButtonLeft,modifier) :: tail => {
      if (coordinateValue.length > 0 && coordinateValue != " " && coordinateValue != "-" && coordinateValue != "." && coordinateValue != "-.")
        End(java.lang.Double.parseDouble(coordinateValue))
      else
        End
      }

      case Start(_ ,i: InputRequest) :: KeyDown(code, _) :: tail => {
        //Ends if enter was pressed...
        if (code == Key.enter) {
          End(0.0)
          //Othervise it starts:
        } else if (code.toChar.isDigit || code.toChar.toString == "-" || code.toChar.toString == ".") {
          coordinateValue += code.toChar
          Siigna display coordinateValue
        }
        inputRequest = Some(i)
        inputType = Some(i.inputType)
        guides = i.guides
        referencePoint = i.referencePoint
      }

      //Read numbers and minus, "." and enter as first entry if no guide is provided:
      case Start(_,_) :: KeyDown(code, _) :: tail => {
        //save the already typed key:
        if (code.toChar.isDigit) coordinateValue += code.toChar
        if (code.toChar.toString == "-" && coordinateValue.length() == 0) coordinateValue += code.toChar
        if (code.toChar.toString == "." && coordinateValue.length() == 0) coordinateValue += code.toChar

        Siigna display coordinateValue

      }

      //Ends on return, TAB - returning value:
      case KeyDown(Key.Enter | Key.Tab , _) :: tail => {
        if (coordinateValue.length > 0) {
            var value = Some(java.lang.Double.parseDouble(coordinateValue))
            End(value.get)
          } else End(0.0)
        }

      //Hints, that "," is not a decimal separator - "." should be used
      case KeyDown(',', _) :: tail => {
        Siigna display "Use . for decimal separation"
      }

      case KeyDown(Key.Backspace, _) :: tail => {
        if (coordinateValue.length > 0) coordinateValue = coordinateValue.substring(0, coordinateValue.length-1)
        //The string must be at least a space - an empty string makes the message-function puke...
        //The string must be at least a space - an empty string makes the message-function puke...
        if (coordinateValue.length == 0) coordinateValue += " "
        Siigna display coordinateValue
      }

      //if point returns a keyDown - that is not previously intercepted
      case KeyDown(code, modifier) :: tail => {
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
        if (coordinateValue.length() == 0) End
      }
    })

  override def paint(g : Graphics, t: TransformationMatrix) {
    //if points are in the process of being typed, then draw the shape dynamically on the basis of what coords are given.
    var input: Option[Double] = None 
    val usefulDoubleAsInput: Boolean =
      // Space, - (minus), . (point) or -. (minus, then point):
      //If the input string ony contains one of these, there is no try to parse the string to a to double, and no guide to be drawn
      if (coordinateValue.length > 0 && coordinateValue != " " && coordinateValue != "-" && coordinateValue != "." && coordinateValue != "-.") {
        input = Some(java.lang.Double.parseDouble(coordinateValue))   
        true
      } else {
        false
      }

    //There will be issues with points typed as offsets on track-guides. This was solved in the old module here - look there for information/inspiration...

    guides.foreach(_ match {
      case DoubleGuide(guide) => {
        if (usefulDoubleAsInput == true && input.get != 0) {
          guide(input.get).foreach(s => g.draw(s.transform(t)))
        }
      }
      case _ => // No double guide
    } )
  }
}