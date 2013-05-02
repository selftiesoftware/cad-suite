/*
 * Copyright (c) 2008-2013. Siigna is released under the creative common license by-nc-sa. You are free
 * to Share — to copy, distribute and transmit the work,
 * to Remix — to adapt the work
 *
 * Under the following conditions:
 * Attribution —  You must attribute the work to http://siigna.com in the manner specified by the author or licensor (but not in any way that suggests that they endorse you or your use of the work).
 * Noncommercial — You may not use this work for commercial purposes.
 * Share Alike — If you alter, transform, or build upon this work, you may distribute the resulting work only under the same or similar license to this one.
 */

package com.siigna.module

import base.{PaperHeader, Menu}
import cad.radialmenu.category.StartCategory
import com.siigna._

/**
 * An init module for the cad-suite.
 */
class ModuleInit extends Module {
  Menu.startCategory = StartCategory

  val header = com.siigna.module.base.PaperHeader

  protected var lastModule : Option[Module] = None

  var nearestShape : Option[(Int, Shape)] = None   //The nearest shape to the current mouse position.
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
      case module => lastModule = Some(module) //enable module recall with space
    }
    Start(m, modText)
  }

  def stateMap = Map(
    'Start -> {
      // Match for modules to forward to
      case End(module : Module) :: tail => {
        lastModule = Some(module) // Store it as a last module

        Start(module) // Forward
      }

      //Rightclick starts menu:
      case MouseDown(_, MouseButtonRight, _) :: tail => {
        textFeedback.inputFeedback("EMPTY") //clear shortcut text guides
        Start('base, "Menu")
      }

      //double click anywhere on a shape selects the full shape.
      case  MouseDown(p2, button, modifier) :: MouseUp(p1 ,MouseButtonLeft , _) :: tail => {
        textFeedback.inputFeedback("EMPTY") //clear shortcut text guides

        if (p1 == p2){
          Start('cad, "Selection", MouseDouble(p2,button,modifier))
        } }

      //Leftclick:
      // 1: Starts select, to select shape part at cursor, if nothing is selected,
      // TODO: or if the click was away from the selection:
      case MouseDown(p, MouseButtonLeft, modifier) :: tail => {
        textFeedback.inputFeedback("EMPTY") //clear shortcut text guides

        //Check if there is a useable selection:
        Start('cad, "Selection", MouseDown(p, MouseButtonLeft, modifier))
      }

      //Leftclick and drag starts :
      // 1: Move if something is selected, and near where the drag starts, or
      // 2: Select, if nothing is selected yet, or if something is selected, but away from the cursor
      // TODO: Change it, so it is if something is not close to the selection - now it if the click
      // is close to any shape in the drawing
      case MouseDrag(p2, button2, modifier2) :: MouseDown(p, button, modifier) :: tail => {
        textFeedback.inputFeedback("EMPTY") //clear shortcut text guides
        if (Drawing.selection.isDefined) {
          val m = p.transform(View.deviceTransformation)
          if (Drawing(m).size > 0) {
            val nearest = Drawing(m).reduceLeft((a, b) => if (a._2.geometry.distanceTo(m) < b._2.geometry.distanceTo(m)) a else b)
            if (nearest._2.distanceTo(m) < Siigna.selectionDistance) {
            Start('cad, "modify.Move", p )
            } else {
            Start('cad, "Selection", MouseDrag(p, button, modifier))
          }} else {
            //Necessary since Drawing(m).size is 0 when marking far away from selected shape/parts
            Start('cad, "Selection", MouseDrag(p, button, modifier))
          }
        } else {
          Start('cad, "Selection", MouseDrag(p, button, modifier))
        }
      }

      // Delete
      case KeyDown(Key.Delete, _) :: tail => {
        shortcut = ""
        if (Drawing.selection.isDefined) {
          Delete(Drawing.selection)
        }
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
      case KeyDown(Key.Esc, _) :: tail => {
        shortcut = ""
        textFeedback.inputFeedback("EMPTY") //clear shortcut text guides
        Drawing.deselect()
      }
      // add or subtract from selections
      case KeyDown(Key.Shift, modifier) :: tail => Start('cad, "Selection", KeyDown(Key.Shift, modifier))
      //End(KeyDown(Key.Shift, modifier))  // Start subtract from selection
      case KeyUp(Key.Shift, modifier) :: tail => End(KeyUp(Key.Shift,modifier))  // Stop subtract from selection

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
      }

      case _ =>
    }
  )

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
  }
}
