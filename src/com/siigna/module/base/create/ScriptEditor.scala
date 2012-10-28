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
import com.siigna.app.model.shape.TextShape
import java.awt.datatransfer._
import java.awt.{Color, Toolkit}

/**
 * A mock-up of a scripting module, that can be used to execute a series of drawing functions quickly.
 *
 * input: The scripting module allows for typing or pasting of a script in plain text.
 * Shapes are created or manipulated on the basis of the script.
 * The script is run and the module ends when ESC is pressed (for now, an Execute button should be implemented)
 *
 * Further elaboration of this module would include:
 *
 * - an examination of whether scala code could be run as a script. That would be sweet!
 * - ability to select Shapes as a starting point for running the script.
 * Eg. picking a line first, then running a script that will copy and rotate it 100 times following set of rules defined in the script.
 */


//TODO: create syntatic sugar for scripting, allowing eg. 'CreateCategory Line (0,0) (100,0)'

class ScriptEditor extends Module{

  val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
  val clipData = clipboard.getContents(clipboard)

  //val setClipboardData = clipboard.setContents(text, "siigna")

  val getClipboardData = {
    if (clipData != null) {
      if (clipData.isDataFlavorSupported(DataFlavor.stringFlavor)) {
        clipData.getTransferData(DataFlavor.stringFlavor).toString
      } else ""
    }
  }

  var bgColor = 0.80f

  var moduleOpen = false
  
  def getNewLine (first : Int, last : Int) = {
    if (first <= last) first - 1
    else last
  }

  var text : String = ""

  def stateMap = Map(
    'Start -> {
      case events => {
        //open/browse script library

        //save script text

        //change the background color of the paper to communicate that Siigna is in Scripting mode
        com.siigna.app.view.View.paperColor = bgColor

        events match {
          case KeyDown(Key.Backspace, _) :: tail => {
            if (text.length != 0) text = text.substring(0, text.length - 1)
          }
          case KeyDown(Key.Enter, _) :: tail => println("do return here")
          case KeyDown(Key.Esc, _) :: tail => {
            'End
          }
          case KeyDown(key, mod) :: tail => {
            //get special keys
            if (key == '8' && mod == ModifierKeys(true, false, false)) text += '('
            else if (key == '9' && mod == ModifierKeys(true, false, false)) text += ')'
            //prevent a space from being added when SHIFT or CTRL is pressed
            else if (key == 16 || key == 17) None
            //paste text
            else if (key == 'v' && mod == ModifierKeys(false, true, false)) text += getClipboardData

            //get regular keys
            else {
              //println(key)
              text += key.toChar.toString.toLowerCase
            }
          }
          //allow menu interaction while module is running
          case MouseUp(point, MouseButtonRight, _) :: tail => {
            if(moduleOpen == false){
              moduleOpen = true
              Module('Menu)
              Controller ! Message(point)
            } else moduleOpen = false
          }
          //TODO: add standard mouse-text interaction. (Highlight, move, etc.)
          case _ =>
        }
        None
      }
    },

    'End -> {
      () => {
        //reset the vars
        com.siigna.app.view.View.paperColor = 1.00f
        text = ""
      }
    }
  )

  override def paint(g : Graphics, t : TransformationMatrix) {

    val scale = Siigna.paperScale
    val textColor = new Color(1.00f, 1.00f, 1.00f, 1.00f)
    val tL = Drawing.boundary.topLeft
    //header
    g draw TextShape("siigna scripting v0.1", tL + Vector2D(5 * scale, -3 * scale), 4 * scale).setAttributes("color" -> textColor, "TextAlignment" -> Vector2D(0,0 )).transform(t)

      //text
     if(!text.isEmpty) g draw TextShape(text, tL + Vector2D(20 * scale,- 20 * scale), 5 * scale).setAttribute("TextAlignment" -> Vector2D(0, 0)).transform(t)

    //accept button

    //draw script output dynamically
  }
}
