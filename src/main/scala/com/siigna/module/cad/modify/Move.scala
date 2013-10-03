/*
* Copyright (c) 2008-2013. Siigna is released under the creative common license by-nc-sa. You are free
* to Share — to copy, distribute and transmit the work,
* to Remix — to adapt the work
*
* Under the following conditions:
* Attribution — You must attribute the work to http://siigna.com in the manner specified by the author or licensor (but not in any way that suggests that they endorse you or your use of the work).
* Noncommercial — You may not use this work for commercial purposes.
* Share Alike — If you alter, transform, or build upon this work, you may distribute the resulting work only under the same or similar license to this one.
*/

package com.siigna.module.cad.modify

import com.siigna._
import app.Siigna
import com.siigna.module.cad.create._
import module.Tooltip

class Move extends Module {
  val origin = Drawing.selection.transformation

  var firstPoint : Option[Vector2D] = None
  var secondPoint : Option[Vector2D] = None

  var transformation : Option[TransformationMatrix] = None

  val vector2DGuideStateOne = Vector2DGuideKeys((v: Vector2D) => {
    transformation = Some(TransformationMatrix(v, 1))
    Drawing.selection.transform(transformation.get)
    val draw = Drawing.selection.shapes.values
    Drawing.selection.transformation = origin //Leave the original for snapping...
    draw
  })

  val vector2DGuide = Vector2DGuide((v: Vector2D) => {
    transformation = Some(TransformationMatrix((v - firstPoint.get), 1))
    Drawing.selection.transform(transformation.get)
    val draw = Drawing.selection.shapes.values
    Drawing.selection.transformation = origin //Leave the original for snapping...
    draw
  })

  val stateMap: StateMap = Map(
    'Start -> {
      //Exit strategy
      case (End | KeyDown(Key.Esc, _) | End(KeyDown(Key.escape, _)) | MouseDown(_, MouseButtonRight, _) | End(MouseDown(_,MouseButtonRight, _)) ) :: tail => End

      //If the move module starts with a point, and there is a selection, it knows where to start...
      case Start(_,v: Vector2D) :: tail => {
        if (Drawing.selection.isEmpty) {
          Siigna display "Select objects to move"
          Tooltip.blockUpdate(3500)
          Start('cad, "Selection")
        } else {
          //change cursor to crosshair
          Siigna.setCursor(Cursors.crosshair)
          //Update tooltip
          Tooltip.updateTooltip("Move tool active")
          //Handle input
          firstPoint = Some(v.transform(View.deviceTransformation))
          //Goto next state
          'StateTwo
        }
      }

      //Handle the situation, where Module Init upon click forwards to select, and
      //Select forwards to move, if the user then drags, and the click was near a shape.
      case Start (_,_) :: End(_) :: MouseDrag(_, _, mod) :: MouseDown(p, _, _) :: tail => {
        println("DragMove begins in move")
        firstPoint = Some(p.transform(View.deviceTransformation))
        'StateTwo
      }

      //Handle enter, backspace and unexpected events
      case (End(KeyDown(Key.enter,_)) | End(KeyDown(Key.backspace,_))) :: tail =>

      //If none of the above happens, module should go on like this:
      case x => {
        //If nothing is selected, start select
        if (Drawing.selection.isEmpty) {
          Siigna display "Select objects to move"
          Tooltip.blockUpdate(3500)
          Start('cad, "Selection")
          //If there is a selection, go to next state
        } else {
          //Update tooltip
          Tooltip.updateTooltip("Move tool active")
          println("Move started, no start point received. Looking for points... Event stream: " + x)
          //Goto next state
          'StateOne
        }
      }
    },

    'StateOne -> {
      //Exit strategy
      case (End | KeyDown(Key.Esc, _) | End(KeyDown(Key.escape, _)) | MouseDown(_, MouseButtonRight, _) | End(MouseDown(_,MouseButtonRight, _)) ) :: tail => End

      //Handle values returned from input

      //Input type 16 returns a Vector2D on left mouse down. It'll be the first point.
      case End(v: Vector2D) :: tail => {
        if(firstPoint.isEmpty) {
          //The first clicked point can be:
          // a): The point where a drag-move starts, or
          // b): The point, where the user "picks up" the selection that should be moved.
          // That depends on whether the mouse is released at the same, or a different point.
          // So, the input-request is only for a mouse-up (input type 8)
          firstPoint = Some(v)
          Start('cad, "create.Input", InputRequest(18,None,vector2DGuide))
        } else if (v == firstPoint.get) {
          //If the received point is the same as the first point, it is the start-point for the move
          //(since mouse-up happened on the same spot as mouse-down). Send an input-request for an end-point:
          Start('cad, "create.Input", InputRequest(5,None,vector2DGuide))
        } else {
          //If the received point is NOT the same as the start point, it is the end-point for the move
          //(since mouse-up, or mouse down, happened on a different spot than the start point). Do the move:
          Drawing.selection.transformation = origin
          transformation = Some(TransformationMatrix((v - firstPoint.get), 1))
          Drawing.selection.transform(transformation.get)
          Drawing.deselect()
          End
        }
      }
      //Input type 16 returns mouse-down-event if a vector is typed by key - if such a vector is typed, it's the move-vector.
      // Do the move and finish the module...
      case End(MouseDown(v: Vector2D,_,_)) :: tail => {
        Drawing.selection.transformation = origin
        transformation = Some(TransformationMatrix(v, 1))
        Drawing.selection.transform(transformation.get)
        Drawing.deselect()
        End
      }

      //Handle enter, backspace and unexpected events
      case (End(KeyDown(Key.enter,_)) | End(KeyDown(Key.backspace,_))) :: tail => //Does nothing

      //Request input
      case _ => Start('cad, "create.Input", InputRequest(16,None,vector2DGuideStateOne) )
    },

    'StateTwo -> {
      //Exit strategy
      case (End | KeyDown(Key.Esc, _) | End(KeyDown(Key.escape, _)) | MouseDown(_, MouseButtonRight, _) | End(MouseDown(_,MouseButtonRight, _)) ) :: tail => End

      //Handle values returned from input:
      case End(v: Vector2D) :: tail => {
        if (v == firstPoint.get) {
          //If the received point is the same as the first point, it is the start-point for the move
          //(since mouse-up happened on the same spot as mouse-down). Send an input-request for an end-point:
          Start('cad, "create.Input", InputRequest(5,None,vector2DGuide))
        } else {
          //If the received point is NOT the same as the start point, it is the end-point for the move
          //(since mouse-up, or mouse down, happened on a different spot than the start point). Do the move:
          Drawing.selection.transformation = origin
          transformation = Some(TransformationMatrix((v - firstPoint.get), 1))
          Drawing.selection.transform(transformation.get)
          Drawing.deselect()
          End
        }
      }

      //Handle enter, backspace and unexpected events
      case (End(KeyDown(Key.enter,_)) | End(KeyDown(Key.backspace,_))) :: tail => //Does nothing
      case _ => Start('cad, "create.Input", InputRequest(18,None,vector2DGuide))
    }
  )
}