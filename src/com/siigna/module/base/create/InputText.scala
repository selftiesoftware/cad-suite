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

class InputText extends Module {

  private var text : String = ""  //input string

  var inputRequest: Option[InputRequest] = None
  var vector2DGuide: Option[Vector2DGuide] = None
  var doubleGuide: Option[DoubleGuide] = None
  var textGuide: Option[TextGuide] = None
  var vector2DMessageGuide: Option[Vector2DMessageGuide] = None
  var doubleMessageGuide: Option[DoubleMessageGuide] = None
  var textMessageGuide: Option[TextMessageGuide] = None
  var referencePoint1: Option[Vector2D] = None
  var referencePoint2: Option[Vector2D] = None
  var referenceDouble: Option[Double] = None
  var inputType: Option[Int] = None


  val stateMap: StateMap = Map(

    'Start -> {
      case MouseDown(p,MouseButtonRight,modifier) :: tail => End(MouseDown(p,MouseButtonRight,modifier))

      case Start(_ ,i: InputRequest) :: KeyDown(code, _) :: tail => {
        inputRequest = Some(i)
        if (!i.vector2DGuide.isEmpty) vector2DGuide = i.vector2DGuide
        if (!i.doubleGuide.isEmpty) doubleGuide = i.doubleGuide
        if (!i.textGuide.isEmpty) textGuide = i.textGuide
        if (!i.vector2DMessageGuide.isEmpty) vector2DMessageGuide = i.vector2DMessageGuide
        if (!i.doubleMessageGuide.isEmpty) doubleMessageGuide = i.doubleMessageGuide
        if (!i.textMessageGuide.isEmpty) textMessageGuide = i.textMessageGuide
        if (!i.referencePoint1.isEmpty) referencePoint1 = i.referencePoint1
        if (!i.referencePoint2.isEmpty) referencePoint2 = i.referencePoint2
        if (!i.referenceDouble.isEmpty) referenceDouble = i.referenceDouble
        if (!i.inputType.isEmpty) inputType = i.inputType
        //save the already typed key:
        if (code.toChar.isValidChar) text += code.toChar
      }

      case Start(_ ,g: TextGuide) :: KeyDown(code, _) :: tail => {
        //textGuide = Some(g.textGuide)
        //inputType = Some(g.inputType)
        //save the already typed key:
        if (code.toChar.isValidChar) text += code.toChar
      }

      //Read numbers and minus, "," and enter as first entry if no guide is provided:
      case Start(_,_) :: KeyDown(code, _) :: tail => {
        //save the already typed key:
        if (code.toChar.isValidChar) text += code.toChar

      }

      //Ends on return, komma, TAB - returning value:
      case KeyDown(Key.Enter | Key.Tab | (','), _) :: tail => {
        if (text.length > 0) {
          End(text)
        }
      }

      case KeyDown(Key.Backspace, _) :: tail => {
        if (text.length > 0) text = text.substring(0, text.length-1)
        //The string must be at least a space - an empty string makes the message-function puke...
        //The string must be at least a space - an empty string makes the message-function puke...
        if (text.length == 0) text += " "
      }

      //if point returns a keyDown - that is not previously intercepted
      case KeyDown(code, modifier) :: tail => {
        if (code == Key.escape)
          End(KeyDown(code,modifier))
        //get the input from the keyboard if it is numbers, (-) or (.)
        val char = code.toChar
        if (char.isValidChar)
          text += char

      }
      case _ => {
      }
    })

  override def paint(g : Graphics, t: TransformationMatrix) {
    //if text the process of being typed, then draw the textshape dynamically.

    if(textGuide.isDefined && text.length > 0){
      textGuide.get.textGuide(text).foreach(s => g.draw(s.transform(t)))
    }
  }
}