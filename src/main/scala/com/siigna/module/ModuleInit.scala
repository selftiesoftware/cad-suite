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

import base.{PaperHeader, Menu}
import com.siigna.module.cad.radialmenu.category.{ModifyCategory, StartCategory}
import com.siigna._
import com.siigna.app.model.selection.EmptySelection
import module.cad.create.InputRequestNew

/**
 * An init module for the cad-suite.
 *
 * MODULE-MAP:
 *
 * legend: L/R  Left/Right
 *         M    Mouse
 *         D/U  Down/Up
 *         S/N  Same/New (point)
 *         P    Point
 *         R    Return to start
 *         M    Menu
 *         I/O  On/Off
 *         C    Char
 *
 *            START
 *
 *         /    |    \
 *        /     |     \
 *      LMD    RMD      CHAR
 *      g       |    /   |    \
 *      |       | del ctrl+c  C
 *     LMU     menu           |
 *     / \                  shortcut
 *   SP  NP
 *   |    |
 *  / \ selBox
 * MO MI
 * |  |
 * R MenuTool
 *
 */
class ModuleInit extends Module {
  Menu.startCategory = StartCategory

  val header = com.siigna.module.base.PaperHeader

  protected var lastModule : Option[Module] = None

  var activeSelection : Selection = EmptySelection   //The nearest shape to the current mouse position.
  var activeSelectionVertices : Traversable[Vector2D] = Set.empty
  var toolSuggestions = List[String]() //a list of possible tools in a given category. activated by shortcuts

  var selectionAlteration = false

  var shortcut = ""
  //draw feedback when typing shortcuts
  val textFeedback = new inputFeedback

  //case KeyDown('a', _) :: KeyUp('c', _) :: tail => shortcutProcess("a", "create.Arc", 'cad)

  //start module and process graphic feedback when a tool shortcut is received
  //input : shortcut, module name, module category
  def shortcutProcess(s : String, modText : String, m : Symbol) = {
    shortcut = s
    toolSuggestions = List[String]() //reset tool suggestions
    textFeedback.inputFeedback(shortcut)//display feedback telling the module is active

    // Sets the latest module and start it
    Module(m, modText) collect {
      case module => {
        println("Module" + module)
        lastModule = Some(module)
      } //enable module recall with space
    }
    shortcut = ""
    Start(m, modText)
  }

  def stateMap = Map(
    'Start -> {

      // Match for modules to forward to
      case End(module : Module) :: tail => {
        lastModule = Some(module) // Store it as a last module
        Start(module) // Forward
      }

      case End(p : Vector2D) :: tail => {
        textFeedback.inputFeedback("EMPTY") //clear shortcut text guides
        Start('cad, "Selection", p)
      }

      case End(MouseDown(p,MouseButtonRight,modifier)) :: tail => {
        textFeedback.inputFeedback("EMPTY") //clear shortcut text guides
        // If any selections are defined we start in the Modify category
        if (Drawing.selection.isDefined) {
          Start('base, "Menu", ModifyCategory)
        } else Start('base, "Menu")
      }

      case End(KeyDown(Key.Space,_)) :: tail => {
        if (lastModule.isDefined) {
          println("Space")
          println("Last module: " + lastModule.get)
          shortcut = ""
          textFeedback.inputFeedback("EMPTY")//clear any active tooltips
          textFeedback.inputFeedback("GETPREVIOUS") //send a command to inputFeedback to display the last module name
          Start(lastModule.get.newInstance)
        }
      }

      case End(KeyDown(code: Int,modifier: ModifierKeys)) :: tail => {
        // Special keys:
        //Delete:
        if (code == Key.Delete) {
          shortcut = ""
          if (Drawing.selection.isDefined) {
            Delete(Drawing.selection)
          }
          //Escape:
        } else if (code == Key.Escape) {
          shortcut = ""
          textFeedback.inputFeedback("EMPTY") //clear shortcut text guides
          Drawing.deselect()
        }

        // Letters or numbers:
        if (code.toChar.isLetterOrDigit == true) {
          val shortcutKey: Char = code.toChar
          println("shortcutKey" + shortcutKey)

          //Modified keys: Control:
          if (modifier.ctrl == true ) {
            if (shortcutKey == 'a') Drawing.selectAll()
            if (shortcutKey == 'c') shortcutProcess("q", "create.Copy", 'cad)
            if (shortcutKey == 'z') Drawing.undo()
            if (shortcutKey == 'y') Drawing.redo()
          }

          //MENU SHORTCUTS - LETTERS:
          if (shortcut == "") {
            if(shortcutKey == 'c' || shortcutKey == 'h' || shortcutKey == 'm' || shortcutKey == 'p') {
              shortcut = shortcutKey.toString
              toolSuggestions = textFeedback.inputFeedback(shortcut)
            }
          } else if (shortcut == "c") {
            if      (shortcutKey == 'a') shortcutProcess("a", "create.Arc", 'cad)
            else if (shortcutKey == 'c') shortcutProcess("c", "create.Circle", 'cad)
            else if (shortcutKey == 'd') shortcutProcess("d", "create.Lineardim", 'cad)
            else if (shortcutKey == 'e') shortcutProcess("e", "create.Explode", 'cad)
            else if (shortcutKey == 'l') shortcutProcess("l", "create.Line", 'cad)
            else if (shortcutKey == 'o') shortcutProcess("o", "create.Offset", 'cad)
            else if (shortcutKey == 'p') shortcutProcess("p", "create.Polyline", 'cad)
            else if (shortcutKey == 'r') shortcutProcess("r", "create.Rectangle", 'cad)
            else if (shortcutKey == 't') shortcutProcess("t", "create.Text", 'cad)
          } else if (shortcut == "h") {
            if      (shortcutKey == 'd')   shortcutProcess("d", "helpers.Distance", 'cad)
            else if (shortcutKey == 's')   shortcutProcess("s", "helpers.SnapToggle", 'cad)
            else if (shortcutKey == 't')   shortcutProcess("t", "helpers.TrackToggle", 'cad)
          } else if (shortcut == "m") {
            if      (shortcutKey == 'm')   shortcutProcess("m", "modify.Move", 'cad)
            else if (shortcutKey == 'r')   shortcutProcess("r", "modify.Rotate", 'cad)
            else if (shortcutKey == 's')   shortcutProcess("s", "modify.Scale", 'cad)
            else if (shortcutKey == 't')   shortcutProcess("t", "modify.Trim", 'cad)
          } else if (shortcut == "p") {
            if      (shortcutKey == 'c')   shortcutProcess("c", "properties.Colors", 'cad)
            else if (shortcutKey == 's')   shortcutProcess("s", "properties.Stroke", 'cad)
          }
        }

      }


      /*

      //Rightclick starts menu:
      case MouseDown(_, MouseButtonRight, _) :: tail => {
        textFeedback.inputFeedback("EMPTY") //clear shortcut text guides

        // If any selections are defined we start in the Modify category
        if (Drawing.selection.isDefined) {
          Start('base, "Menu", ModifyCategory)
        } else Start('base, "Menu")
      }




      // Make sure we do not return to the selection immediately after it ended
      case End :: MouseDown(_, _, _) :: tail =>



      //Leftclick:
      // 1: Starts select, to select shape part at cursor, if nothing is selected,
      // TODO: or if the click was away from the selection:
      case MouseDown(p, MouseButtonLeft, modifier) :: tail => {
        textFeedback.inputFeedback("EMPTY") //clear shortcut text guides

        //Check if there is a useable selection:
        Start('cad, "Selection")
      }

      // Delete
      case KeyDown(Key.Delete, _) :: tail => {
        shortcut = ""
        if (Drawing.selection.isDefined) {
          Delete(Drawing.selection)
        }
      }

      // Highlight active shape(s)
      case MouseMove(p, _, _) :: tail => {
        val point = p.transform(View.deviceTransformation)
        val shapes = Drawing(point)
        activeSelection = Selection(shapes.map(t => t._1 -> (t._2 -> t._2.getSelector(point))))
        activeSelectionVertices = activeSelection.shapes.values.flatMap(s => s.getVertices(s.getSelector(point)))
      }

      //shortcuts

      //create
      case KeyDown('a', _) :: KeyUp('c', _) :: tail => shortcutProcess("a", "create.Arc", 'cad)
      case KeyDown('c', _) :: KeyUp('c', _) :: tail => shortcutProcess("c", "create.Circle", 'cad)
      case KeyDown('d', _) :: KeyUp('c', _) :: tail => shortcutProcess("d", "create.Lineardim", 'cad)
      case KeyDown('e', _) :: KeyUp('c', _) :: tail => shortcutProcess("e", "create.Explode", 'cad)
      case KeyDown('l', _) :: KeyUp('c', _) :: tail => shortcutProcess("l", "create.Line", 'cad)
      case KeyDown('o', _) :: KeyUp('c', _) :: tail => shortcutProcess("o", "create.Offset", 'cad)
      case KeyDown('p', _) :: KeyUp('c', _) :: tail => shortcutProcess("p", "create.Polyline", 'cad)
      case KeyDown('r', _) :: KeyUp('c', _) :: tail => shortcutProcess("r", "create.Rectangle", 'cad)
      case KeyDown('t', _) :: KeyUp('c', _) :: tail => shortcutProcess("t", "create.Text", 'cad)

      //helpers
      case KeyDown('d', _) :: KeyUp('h', _) :: tail => shortcutProcess("d", "helpers.Distance", 'cad)
      case KeyDown('s', _) :: KeyUp('h', _) :: tail => shortcutProcess("s", "helpers.SnapToggle", 'cad)
      case KeyDown('t', _) :: KeyUp('h', _) :: tail => shortcutProcess("t", "helpers.TrackToggle", 'cad)

      //MODIFY
      case KeyDown('m', _) :: KeyUp('m', _) :: tail => shortcutProcess("m", "modify.Move", 'cad)
      case KeyDown('r', _) :: KeyUp('m', _) :: tail => shortcutProcess("r", "modify.Rotate", 'cad)
      case KeyDown('s', _) :: KeyUp('m', _) :: tail => shortcutProcess("s", "modify.Scale", 'cad)
      case KeyDown('t', _) :: KeyUp('m', _) :: tail => shortcutProcess("t", "modify.Trim", 'cad)

      //PROPERTIES
      case KeyDown('c', _) :: KeyUp('p', _) :: tail => shortcutProcess("c", "properties.Colors", 'cad)
      case KeyDown('s', _) :: KeyUp('p', _) :: tail => shortcutProcess("s", "properties.Stroke", 'cad)

      case KeyDown('a', Control) :: tail => Drawing.selectAll()
      case KeyDown('c', Control) :: tail => shortcutProcess("q", "create.Copy", 'cad)
      case KeyDown('z', Control) :: tail => Drawing.undo()
      case KeyDown('y', Control) :: tail => Drawing.redo()

      // Forward to the last initiated module
      case KeyDown(Key.Space, _) :: tail => if (lastModule.isDefined) {
        textFeedback.inputFeedback("EMPTY")//clear any active tooltips
        textFeedback.inputFeedback("GETPREVIOUS") //send a command to inputFeedback to display the last module name
        Start(lastModule.get.newInstance)
      }

      // Release all selections
      case KeyDown((Key.Esc | Key.Enter), _) :: tail => {
        shortcut = ""
        textFeedback.inputFeedback("EMPTY") //clear shortcut text guides
        Drawing.deselect()
      }

      //MENU SHORTCUTS
      case KeyDown('c', _) :: tail => {
        shortcut = "c"
        toolSuggestions = textFeedback.inputFeedback(shortcut)
      }
      case KeyDown('h', _) :: tail => {
        shortcut = "h"
        toolSuggestions = textFeedback.inputFeedback(shortcut)
      }
      case KeyDown('m', _) :: tail => {
        shortcut = "m"
        toolSuggestions = textFeedback.inputFeedback(shortcut)
      }
      case KeyDown('p', _) :: tail => {
        shortcut = "p"
        toolSuggestions = textFeedback.inputFeedback(shortcut)
      } */

      case _ => {
        Start('cad,"create.InputNew", InputRequestNew(14,None))

      }
    }
  )

  private val selectionAttributes = Attributes("StrokeWidth" -> 0.7, "Color" -> Siigna.color("colorSelected").getOrElse("#AAAAAA"))

  override def paint(g : Graphics, t : TransformationMatrix) {
    g draw PaperHeader.openness.transform(t) //color to show level of openness
    g draw PaperHeader.headerFrame.transform(t) //frame around drawing info
    g draw PaperHeader.scaleText.transform(t) //frame around drawing info

    //draw tool shourcut suggestions
    if(!shortcut.isEmpty) {
      val s = textFeedback.paintSuggestions(toolSuggestions)
      for (i <- 0 to s.size -1) {
        g draw s(i)
      }  
    }

    activeSelection.parts.foreach(s => g draw s.setAttributes(selectionAttributes).transform(t))
    activeSelectionVertices.foreach(v => g draw v.transform(t))
  }
}
