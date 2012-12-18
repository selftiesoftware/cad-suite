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

import com.siigna._

/**
 * a class to add visual feedback when typing shortcuts
 */

class inputFeedback {
  var category: Option[String] = None
  var command: Option[String] = None

  //a function to parse input chars and provide a textShape with visual aid showing possible commands / tools
  def suggestion(s: String) = {
    println("AA")
    //suggested = "create" //the var needs to be set for the s match to work. weird.
    s match {
      //MENUS
      case "c" => {
        category = Some("create")
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
      case "a" => {
        if (category == Some("create")) command = Some("arc")
      }
      case "l" => {
        if (category == Some("create")) command = Some("line")
      }
      case "o" => {
        if (category == Some("create")) command = Some("offset")
      }
      case "r" => {
        if (category == Some("create")) command = Some("rectangle")
      }
      case "t" => {
        if (category == Some("modify")) command = Some("trim")
      }
    }
    if (category.isDefined && !command.isDefined) Siigna display category.get
    else if (category.isDefined && command.isDefined) {
      Siigna display (category.get + " " + command.get)
      command = None
    }
    else Siigna display ("type shortcut for tools by category: C, H, M, or P")
  }
}
