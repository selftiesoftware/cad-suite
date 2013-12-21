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
import cad.create.InputRequest
import cad.setPaperProperties
import com.siigna.module.cad.radialmenu.category.{EditCategory, StartCategory}
import com.siigna._
import com.siigna.app.model.selection.EmptySelection
import com.siigna.module.cad.create.InputRequest
import java.awt.Cursor
import scala.Some

/**
 * An init module for the cad-suite.
 *
 * MODULE-MAP:
 *
 * legend: L/R  Left/Right
 * M    Mouse
 * D/U  Down/Up
 * R    Return to start
 * M    Menu
 * I/O  On/Off
 * C    Char
 *
 * START
 *
 * /      |      \
 * /       |       \
 * LMD      RMD      CHAR
 * |        |        |
 * SELECT      |        C -- ESC, Space,
 * menu      |
 * CHAR
 * |
 * Shortcut
 *
 */
class ModuleInit extends Module {
  Menu.startCategory = StartCategory

  //call the paper header object; it displays the drawing title and paper scale in the lower right corner of the paper.
  val header = com.siigna.module.base.PaperHeader

  Siigna tooltip(List("Right click to open menu","Here you'll find the drawing tools", "Suggestions for improvements? Please use the comment box above"))

  protected var lastModule: Option[Module] = None

  var activeSelection: Selection = EmptySelection
  //The nearest shape to the current mouse position.
  var activeSelectionVertices: Traversable[Vector2D] = Set.empty

  //define the default cursor
  val defaultCursor = new Cursor(Cursor.DEFAULT_CURSOR)

  var toolSuggestions = List[String]() //a list of possible tools in a given category. activated by shortcuts

  var selectionAlteration = false

  var shortcut = ""
  //draw feedback when typing shortcuts
  val textFeedback = new inputFeedback

  //case KeyDown('a', _) :: KeyUp('c', _) :: tail => shortcutProcess("a", "create.Arc", 'cad)

  //start module and process graphic feedback when a tool shortcut is received
  //input : shortcut, module name, module category
  def shortcutProcess(s: String, modText: String, m: Symbol) = {

    shortcut = s
    toolSuggestions = List[String]() //reset tool suggestions
    textFeedback.inputFeedback(shortcut) //display feedback telling the module is active

    // Sets the latest module and start it
    Module(m, modText) collect {
      case module => {
        lastModule = Some(module)
      } //enable module recall with space
    }
    shortcut = ""
    Start(m, modText)
  }

  def startMenu = {
    textFeedback.inputFeedback("EMPTY") //clear shortcut text guides
    // If any selections are defined we start in the Modify category
    if (Drawing.selection.isDefined) {
      Start('base, "Menu", EditCategory)
    } else Start('base, "Menu")
  }

  def startPrevious = {
    if (lastModule.isDefined) {
      shortcut = ""
      textFeedback.inputFeedback("EMPTY") //clear any active tooltips
      textFeedback.inputFeedback("GETPREVIOUS") //send a command to inputFeedback to display the last module name
      Start(lastModule.get.newInstance)
    }
  }


  def handleKeyDown(code: Int, modifier: ModifierKeys) = {
    // Special keys:
    //Delete:
    if (code == Key.Delete) {
      shortcut = ""
      if (Drawing.selection.isDefined) {
        //if the bounding box of an imageBackground is deleted, clear the image:
        if(Siigna.imageBackground._2.isDefined && Drawing.selection.contains(Siigna.imageBackground._2.get)) {
          Siigna.imageBackground = (None,None,1.0)
        }
        //once it is tested that no image should be deleted, delete other shapes, if any:
        if (Drawing.selection.isDefined) Delete(Drawing.selection)
      }
      //Escape:
    } else if (code == Key.Escape) {
      shortcut = ""
      textFeedback.inputFeedback("EMPTY") //clear shortcut text guides
      Drawing.deselect()
    }

    // Letters or numbers:
    if (code.toChar.isLetterOrDigit) {
      val shortcutKey: Char = code.toChar

      //Modified keys: Control:
      if (modifier.ctrl) {
        if (shortcutKey == 'a') Drawing.selectAll()
        else if (shortcutKey == 'c') shortcutProcess("c", "create.Copy", 'cad)
        else if (shortcutKey == 'v') shortcutProcess("c", "create.Paste", 'cad)
        else if (shortcutKey == 'z') Drawing.undo()
        else if (shortcutKey == 'y') Drawing.redo()
      } else if (shortcut == "") {
        //MENU SHORTCUTS - LETTERS:
        if (shortcutKey == 'c' || shortcutKey == 'h' || shortcutKey == 'e' || shortcutKey == 'f') {
          shortcut = shortcutKey.toString
          toolSuggestions = textFeedback.inputFeedback(shortcut)
        }
        //COMMONLY USED SHORTCUTS
        else if (shortcutKey == 'l')shortcutProcess("l", "create.Polyline", 'cad) //polyline
        else if (shortcutKey == 'm') shortcutProcess("m", "edit.Move", 'cad) //move
        else if (shortcutKey == 't') shortcutProcess("t", "edit.Trim", 'cad) //trim

        //CREATE shortcuts
      } else if (shortcut == "c") {
        if (shortcutKey == 'a') shortcutProcess("a", "create.Arc", 'cad)
        else if (shortcutKey == 'c') shortcutProcess("c", "create.Circle", 'cad)
        else if (shortcutKey == 'd') shortcutProcess("d", "create.Lineardim", 'cad)
        else if (shortcutKey == 'l') shortcutProcess("l", "create.Line", 'cad)
        else if (shortcutKey == 'o') shortcutProcess("o", "create.Offset", 'cad)
        else if (shortcutKey == 'p') shortcutProcess("p", "create.Polyline", 'cad)
        else if (shortcutKey == 'r') shortcutProcess("r", "create.Rectangle", 'cad)
        else if (shortcutKey == 't') shortcutProcess("t", "create.Text", 'cad)
        //HELPERS shortcuts
      } else if (shortcut == "h") {
        if (shortcutKey == 'd') shortcutProcess("d", "helpers.Distance", 'cad)
        else if (shortcutKey == 'h') shortcutProcess("h", "helpers.TooltipToggle", 'cad)
        else if (shortcutKey == 's') shortcutProcess("s", "helpers.SnapToggle", 'cad)
        else if (shortcutKey == 't') shortcutProcess("t", "helpers.TrackToggle", 'cad)
        else if (shortcutKey == 'z') shortcutProcess("z", "helpers.ZoomExtends", 'cad)
        //EDIT shortcuts
      } else if (shortcut == "e") {
        if (shortcutKey == 'c') shortcutProcess("c", "edit.Colors", 'cad)
        else if (shortcutKey == 'e') shortcutProcess("e", "edit.Explode", 'cad)
        else if (shortcutKey == 'j') shortcutProcess("j", "edit.Join", 'cad)
        else if (shortcutKey == 'm') shortcutProcess("m", "edit.Move", 'cad)
        else if (shortcutKey == 'r') shortcutProcess("r", "edit.Rotate", 'cad)
        else if (shortcutKey == 's') shortcutProcess("s", "edit.Scale", 'cad)
        else if (shortcutKey == 't') shortcutProcess("t", "edit.Trim", 'cad)
        else if (shortcutKey == 'w') shortcutProcess("w", "edit.Stroke", 'cad)

      } else if (shortcut == "f") {
        if (shortcutKey == 'e') shortcutProcess("e", "Export", 'porter)
        else if (shortcutKey == 'i') shortcutProcess("i", "Import", 'porter)
      }
    }
  }

  def stateMap = Map(
    'Start -> {

      // Match for modules to forward to
      case End(module: Module) :: tail => {
        lastModule = Some(module) // Store it as a last module
        Start(module) // Forward
      }

      // Menu
      case MouseDown(p, MouseButtonRight, modifier) :: tail => {
        Tooltip.updateTooltip(List("Choose your tool. Drawing tools are in the Create menu at 12 o'clock","",""))
        shortcut = ""
        textFeedback.inputFeedback("EMPTY") //clear shortcut text guides
        startMenu
      }
      case End(MouseDown(p, MouseButtonRight, modifier)) :: tail => {
        Tooltip.updateTooltip(List("Choose your tool. Drawing tools are in the Create menu at 12 o'clock","",""))
        startMenu
      }

      // Selection
      case End(p: Vector2D) :: tail => {
        textFeedback.inputFeedback("EMPTY") //clear shortcut text guides
        shortcut = ""
        Start('cad, "Selection", p)
      }
      case MouseDown(p: Vector2D, _, _) :: tail => {
        //check if the setPaperProperties function should run.
        if (setPaperProperties.paperChangeCheck(p, true)._1) {
          //if the user has chosen to set the paper scale, goto the SetPaperScale module
          if(setPaperProperties.paperChangeCheck(p, true)._3) Start('cad,"SetPaperScale") else null
        }
        //if not, the user is starting a selection:
        else {
          textFeedback.inputFeedback("EMPTY") //clear shortcut text guides
          shortcut = ""
          Start('cad, "Selection", p)
        }
      }
      case MouseDrag(p: Vector2D, MouseButtonLeft, m1) :: tail => {
        shortcut = ""
        textFeedback.inputFeedback("EMPTY") //clear shortcut text guides
        Start('cad, "Selection", MouseDrag(p,MouseButtonLeft,m1))
      }

      // Start previous
      case KeyDown(Key.Space, _) :: tail => startPrevious
      case End(KeyDown(Key.Space, _)) :: tail => startPrevious

      case KeyDown(code: Int, modifier: ModifierKeys) :: tail => handleKeyDown(code, modifier)
      case End(KeyDown(code: Int, modifier: ModifierKeys)) :: tail => handleKeyDown(code, modifier)

      //If the ending module sent a message (fx. measure distance; lines exploded, of whatever, don't overwrite it...
      case End :: tail => if(Tooltip.lastUpdate +500 > System.currentTimeMillis()) Tooltip.blockUpdate(3500)

      case y => {

        //revert to the arrow-type cursor
        Siigna.setCursor(defaultCursor)
        Tooltip.updateTooltip(List("Click right key to access drawing tools","Keyboard shortcuts: C = Create, H = Helpers, E = Edit, F = File.","SPACE = last tool, ALT = pan."))
        if (Drawing.size < 2) {
          Siigna tooltip(List("Right click to open menu","Here you'll find the drawing tools", "Suggestions for improvements? Please use the comment box above"))
        } else Siigna tooltip(List("Suggestions for improvements? Please use the comment box above","",""))
        Start('cad, "create.Input", InputRequest(14, None))
      }
    }
  )

  private val selectionAttributes = Attributes("StrokeWidth" -> 0.7, "Color" -> Siigna.color("colorSelected").getOrElse("#AAAAAA"))

  override def paint(g: Graphics, t: TransformationMatrix) {

    g draw PaperHeader.openness.transform(t) //color to show level of openness
    g draw PaperHeader.headerFrame.transform(t) //frame around drawing info
    g draw PaperHeader.scaleText.transform(t) //drawing scale text
    g draw PaperHeader.sizeArrows.transform(t) //arrows showing click spots for changing paper size
    g draw PaperHeader.scaleAuto.transform(t) //an "A" showing where to click in order to set drawing scale automaticlly

    //draw tool shoutcut suggestions
    if (!shortcut.isEmpty && textFeedback.category.isDefined) {
      val s = textFeedback.paintSuggestions(toolSuggestions)
      for (i <- 0 to s.size - 1) {
        g draw s(i)
      }
    }

    //println("active selection in mod init: "+activeSelection)

    activeSelection.parts.foreach(s => g draw s.setAttributes(selectionAttributes).transform(t))
    activeSelectionVertices.foreach(v => g draw v.transform(t))
  }
}

object Tooltip {
  var tooltip : List[String] = List("Welcome to Siigna.","Right click to open the menu.","These tips may be disabled in the Helpers category")
  private var baseTime: Long = System.currentTimeMillis()
  private var delay : Int = 0
  private var block : Int = 1000
  var lastUpdate: Long = System.currentTimeMillis()

  Siigna tooltip(tooltip)

  def blockUpdate(milliseconds: Int) {
    block = milliseconds
    baseTime = System.currentTimeMillis()
  }

  def refresh() {
    if (System.currentTimeMillis() > baseTime + block) {
      //Siigna tooltip(List("Welcome to Siigna. Right click to access drawing tools."))
    }
  }

  def updateTooltip(strings : List[String]) {
    while (System.currentTimeMillis() < baseTime + delay) {
      println(List("Waiting for message to be displayed before updating tooltip"))
    }
    if (System.currentTimeMillis() > baseTime + block) {
      lastUpdate = System.currentTimeMillis()
      Siigna tooltip(strings) //show state feedback
    }
  }
}