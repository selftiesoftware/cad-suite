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

package com.siigna.module

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

import java.awt.Color
import com.siigna.app.view._
import com.siigna.app.model.shape.TextShape
import com.siigna.app.Siigna
import com.siigna.util.geom.Vector2D

/**
 * a class to add visual feedback when typing shortcuts
 */

class inputFeedback {

  var backgroundColor = new Color(0f, 0f, 0f, 0.1f)
  var category: Option[String] = None
  var command: Option[String] = None
  var getPrevious = false
  var previous = ""

  def paintFrame(graphics : Graphics, width : Int, height : Int, color : Color = backgroundColor) {
    val center = View.center
    graphics setColor color
    graphics.AWTGraphics.fillRoundRect(center.x.toInt - width / 2, center.y.toInt - height / 2, width, height, 20, 20)
  }


  //a function to parse input chars and provide a textShape with visual aid showing possible commands / tools
  def inputFeedback(s: String) = {
    var suggestions = List[String]()

    s match {
      //MENUS
      case "c" => {
        if (category == Some("create")) command = Some("circle")
        else {
          category = Some("create")
          suggestions = List ("A - arc", "C - circle", "D - linear dimension","E - explode","L - line","P - polyline","O - offset","R - rectangle", "T - text")
        }
      }
      case "h" => {
        category = Some("helpers")
        suggestions = List ("D - distance", "S - snap on/off", "T - track on/off")

      }
      case "m" => {
        if (category == Some("modify")) command = Some("move")
        else {
          category = Some("modify")
          suggestions = List ("M - move", "R - rotate", "S - scale")
        }
      }
      case "p" => {
        if (category == Some("create"))  command = Some("polyline")
          else {
          category = Some("properties")
          suggestions = List ("C - colors", "S - stroke")
        }
      }

      //COMMANDS  //TODO: tie these to the module names in moduleInit?
      case "a" => if (category == Some("create")) command = Some("arc")
      case "d" => if (category == Some("create")) command = Some("linear dimension")
      case "e" => if (category == Some("create")) command = Some("explode")
      case "l" => if (category == Some("create")) command = Some("line")
      case "o" => if (category == Some("create")) command = Some("offset")
      case "r" => if (category == Some("create")) command = Some("rectangle")
      case "t" => if (category == Some("create")) command = Some("text")
      case "q" => if (category == Some("create")) command = Some("copy")


      case "GETPREVIOUS" => {
        getPrevious = true
      }
      case "EMPTY" => {
        category = None
      }
      //case "t" => if (category == Some("modify")) command = Some("trim")
      case e => println("received unknown shortcut in inputFeedback: "+e)
    }
    //if one shortcut is typed, display the category
    if (category.isDefined && !command.isDefined) Siigna display (category.get)
    //if two letters are typed, display both the category and the command.
    else if (category.isDefined && command.isDefined) {
      Siigna display (category.get + " " + command.get)
      previous = (category.get + " " + command.get)
      category = None
      command = None
    }
    else if (getPrevious == true) {
      Siigna display previous
      getPrevious = false
    } //display the previous command
    else None//display ("type shortcut for tools by category: C, H, M, or P")
    suggestions //return the suggested tools
  }
  //TODO: draw background fill
  def paintSuggestions(s : List[String]) : List[TextShape] = {
    var list = List[TextShape]()
    for (i <- 0 to s.size -1) {
      // Define the text shape, draw the frame and draw the text
      val text = TextShape(s(i), (View.center - Vector2D(0,-16 - (16 * i+1))), 9)
      list = list :+ text
    }
    list
  }
}
