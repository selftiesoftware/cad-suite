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
import app.Siigna
import module.Tooltip

/**
 * A line module (draws one line-segment)
 */
class Line extends Module {

  private val attributes = {
    val color = Siigna.color("activeColor")
    val lineWidth = Siigna.double("activeLineWidth")
    Attributes(Seq(color.map(c => "Color" -> color.getOrElse(None)), lineWidth.map(w => "StrokeWidth" -> lineWidth.getOrElse(None))).flatten)
  }

  private var startPoint: Option[Vector2D] = None

  private val vector2DGuide = Vector2DGuide((v : Vector2D) => {
    Traversable(LineShape(startPoint.get, v).addAttributes(attributes))
  })

  val stateMap: StateMap = Map(

    'Start -> {
      case _ => {
        //change cursor to crosshair
        Siigna.setCursor(Cursors.crosshair)
        //Update tooltip
        Tooltip.updateTooltip("Line tool active")
        //Request input
        Start('cad, "create.Input", InputRequest(6,None))
        //Goto next state
        'ReceiveFirstPoint
      }
    },

    'ReceiveFirstPoint -> {
      //Exit strategy
      case (KeyDown(Key.Esc, _) | End(KeyDown(Key.escape, _)) | MouseDown(_, MouseButtonRight, _) | End(MouseDown(_,MouseButtonRight, _)) ) :: tail => End

      //Handle values returned from input
      case End(v : Vector2D) :: tail => {
        startPoint = Some(v)
        Start('cad,"create.Input", InputRequest(7,startPoint,vector2DGuide))
        'ReceiveLastPoint
      }

      //Handle enter, backspace and unexpected events
      case (End(KeyDown(Key.enter,_)) | End(KeyDown(Key.backspace,_))) :: tail => {
        Start('cad, "create.Input", InputRequest(6,None))
      }
      case x => {
        println("Line module can't interpret this: " + x)
        Start('cad, "create.Input", InputRequest(6,None))
      }
    },
    'ReceiveLastPoint -> {
      //Exit strategy
      case (KeyDown(Key.Esc, _) | End(KeyDown(Key.escape, _)) | MouseDown(_, MouseButtonRight, _) | End(MouseDown(_,MouseButtonRight, _)) ) :: tail => End

      //Handle values returned from input
      case End(v : Vector2D) :: tail => {
        val line = LineShape(startPoint.get,v).addAttributes(attributes)
        Create(line)
        startPoint = None
        End
      }

      //Handle enter, backspace and unexpected events
      case End(KeyDown(Key.enter,modifier)) :: tail => {
        Start('cad,"create.Input", InputRequest(7,startPoint,vector2DGuide))
      }
      case End(KeyDown(Key.backspace,modifier)) :: tail => {
        startPoint = None
        Start('cad, "create.Input", InputRequest(6,None))
        'ReceiveFirstPoint
      }
      case x => {
        println("Line module can't interpret this: " + x)
        Start('cad,"create.Input", InputRequest(7,startPoint,vector2DGuide))
      }
    }
  )
}