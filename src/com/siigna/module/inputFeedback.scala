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
import com.siigna.util.collection.Attributes
import com.siigna.app.Siigna
import com.siigna.util.geom.Vector2D

/**
 * a class to add visual feedback when typing shortcuts
 */

class inputFeedback {

  var backgroundColor = new Color(0f, 0f, 0f, 0.1f)
  var category: Option[String] = None
  private var color = new Color(0, 0, 0, 255);
  var command: Option[String] = None

  def paintFrame(graphics : Graphics, width : Int, height : Int, color : Color = backgroundColor) {
    val center = View.center
    graphics setColor color
    graphics.g.fillRoundRect(center.x.toInt - width / 2, center.y.toInt - height / 2, width, height, 20, 20)
  }


  //a function to parse input chars and provide a textShape with visual aid showing possible commands / tools
  def inputFeedback(s: String) = {
    var suggestions = List[String]()
    s match {
      //MENUS
      case "c" => {
        category = Some("create")
        println("CCCCC")
        suggestions = List ("A - arc", "C - circle", "D - linear dimension","E - explode","L - line","P - polyline","O - offset","r - rectangle", "t - text")

      }
      case "h" => {
        category = Some("helpers")
      }
      case "m" => {
        category = Some("modify")
      }
      case "p" => {
        if (category == Some("create")) {
          command = Some("polyline")
        } else category = Some("properties")
      }

      //COMMANDS
      case "a" => if (category == Some("create")) command = Some("arc")
      case "e" => if (category == Some("create")) command = Some("explode")
      case "l" => if (category == Some("create")) command = Some("line")
      case "o" => if (category == Some("create")) command = Some("offset")
      case "r" => if (category == Some("create")) command = Some("rectangle")
      case "t" => if (category == Some("modify")) command = Some("trim")

    }
    //if one shortcut is typed, display the category
    if (category.isDefined && !command.isDefined) Siigna display (category.get)
    //if two letters are typed, display both the category and the command.
    else if (category.isDefined && command.isDefined) {
      Siigna display (category.get + " " + command.get)
      command = None
    }
    else None//display ("type shortcut for tools by category: C, H, M, or P")
    suggestions //return the suggested tools
  }
  def paintSuggestions(s : List[String]) : List[TextShape] = {
    var list = List[TextShape]()
      for (i <- 0 to s.size -1) {
        // Define the text shape, draw the frame and draw the text
        val text = TextShape(s(i), (View.center - Vector2D(0,-20 - (20 * i+1))), 10)
        list = list :+ text
    }
    list
  }
}
