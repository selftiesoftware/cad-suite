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

/**
 * A module that collect a pair of digits on the basis of key inputs.
 * Used by modules that need eg. an X and Y coordinate to define a point.
 */

class InputCharacter extends Module {

  private var text : String = ""  //input string

  //Information received from calling module
  var inputRequest: Option[InputRequestNew] = None
  var inputType: Option[Int] = None
  var guides: Seq[Guide] = Seq()
  var referencePoint: Option[Vector2D] = None


  val stateMap: StateMap = Map(

    'Start -> {
      case MouseDown(p,MouseButtonRight,modifier) :: tail => End(MouseDown(p,MouseButtonRight,modifier))

      case Start(_ ,i: InputRequestNew) :: KeyDown(code, modifier) :: tail => {
        inputRequest = Some(i)
        inputType = Some(i.inputType)
        guides = i.guides
        referencePoint = i.referencePoint
        //Return the key:
        if (code.toChar.isLetterOrDigit) End(KeyDown(code, modifier))
        else if (code == Key.delete) End(KeyDown(code, modifier))
        else if (code == Key.space) End(KeyDown(code, modifier))
        else End
      }

      //Read first entry if no guide is provided:
      case Start(_,_) :: KeyDown(code, modifier) :: tail => {
        if (code.toChar.isLetterOrDigit) End(KeyDown(code, modifier))
        else if (code == Key.delete) End(KeyDown(code, modifier))
        else if (code == Key.space) End(KeyDown(code, modifier))
        else End
      }

      case _ => {
        println("Stuck in InputChar")
      }
    })

}

