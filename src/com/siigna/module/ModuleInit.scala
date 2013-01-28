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
import app.model.shape.FullShapePart

/**
 * An init module for the cad-suite.
 */
class ModuleInit extends Module {
  Menu.startCategory = StartCategory

  val header = com.siigna.module.base.PaperHeader

  protected var lastModule : Option[Module] = None

  var nearestShape : Option[(Int, Shape)] = None   //The nearest shape to the current mouse position.
  var toolSuggestions = List[String]() //a list of possible tools in a given category. activated by shortcuts
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

  //Check if there is a useable selection:
  // TODO: Make a more elegant way to check for usable selection - in mainline?
  def usableSelectionExists = {
    var usableSelectionExists: Boolean = false
    if (!Drawing.selection.isEmpty) {
      //The selection could be an empty map, which is unusable - check for that:
      if (Drawing.selection.get.self.size != 0)
      //The map could contain an EmptyShapePart or a Part with
      // an empty bit-set, which is unusable - check for that:
        Drawing.selection.get.self.foreach((shape) => {
          shape._2 match {
            //A FullShapePart or a selector containing a BitSet means a useable selection:
            case FullShapePart => usableSelectionExists = true
            case app.model.shape.PolylineShape.Part(x) => {
              if (x.size >0) {
                //If the size of the bitset is larger than 0, something useful is selected...
                usableSelectionExists = true
              }
            }
            case app.model.shape.LineShape.Part(x) => {
              //If the selector exists, something useful is selected...
              usableSelectionExists = true
            }
            case app.model.shape.CircleShape.Part(x) => {
              //If the selector exists, something useful is selected...
              usableSelectionExists = true
            }
            case app.model.shape.GroupShape.Part(x) => if (x.size >0) {
              //If the bitset is larger than 0, something useful is selected...
              usableSelectionExists = true
            }
            case app.model.shape.TextShape.Part(x) => {
              //If the bitset is larger than 0, something useful is selected...
              usableSelectionExists = true
            }
            case _ =>
          }
        })
    }
    (usableSelectionExists)
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
        if (usableSelectionExists == true) {
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
        if (usableSelectionExists) {
          Delete(Drawing.selection.get.self)
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
        //textFeedback.inputFeedback("EMPTY")//clear any active tooltips
        //textFeedback.inputFeedback("GETPREVIOUS") //send a command to inputFeedback to display the last module name
        //Start(lastModule.get)
      }

      // Release all selections
      case KeyDown(Key.Esc, _) :: tail => {
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
    //construct header elements
    //val scale = paperHeader.scale
    //val unitX = headerShapes.unitX(4)

    //g draw header.horizontal(t) // Draw horizontal headerborder
    //g draw header.vertical(t) //Draw vertical headerborder
    //g.draw(headerShapes.getURL.transform(transformation.translate(scale.boundary.topRight + unitX))) //g draw separator
    //g.draw(scale.transform(paperHeader.transformation(t)))   //draw paperScale

    //draw highlighted vertices and segments that are selectable (close to the mouse)
    if (nearestShape.isDefined) {
      val shape  = nearestShape.get._2
      val part = shape.getPart(mousePosition)
      val points = shape.getVertices(part)
      points.foreach(p => g.draw(t.transform(p)))

      //TODO: activate this -> implement adding attributes to parts in mainline
      //g draw part.setAttributes("Color" -> "#22FFFF".color, "StrokeWidth" -> 1.0).transform(t)
    }
  }
}
