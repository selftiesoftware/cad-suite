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


  var textGuide : Option[String => Traversable[Shape]] = None
  var inputType: Option[Int] = None

  val stateMap: StateMap = Map(

    'Start -> {

      case Start(_ ,g: TextGuide) :: KeyDown(code, _) :: tail => {
        textGuide = Some(g.textGuide)
        inputType = Some(g.inputType)
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
      case KeyDown(code, _) :: tail => {
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
      textGuide.foreach(_(text).foreach(s => g.draw(s.transform(t))))
    }
  }
}