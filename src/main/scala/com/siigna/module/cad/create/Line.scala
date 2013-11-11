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

  private var firstPoint: Option[Vector2D] = None
  private var firstPointSet : Boolean = false

  private val vector2DGuide = Vector2DGuide((v : Vector2D) => {
    Traversable(LineShape(firstPoint.get, v).addAttributes(attributes))
  })

  val stateMap: StateMap = Map(

    'Start -> {
      case End(v : Vector2D) :: tail =>
      case _ => {
        //change cursor to crosshair
        Siigna.setCursor(Cursors.crosshair)
        //Update tooltip
        Tooltip.updateTooltip(List("Line tool active"))
        //Goto next state
        'ReceiveFirstPoint
      }
    },

    'ReceiveFirstPoint -> {
      //Exit strategy
      case (End | KeyDown(Key.Esc, _) | End(KeyDown(Key.escape, _)) | MouseDown(_, MouseButtonRight, _) | End(MouseDown(_,MouseButtonRight, _)) ) :: tail => End

      //Handle values returned from input
        case End(MouseDown(v : Vector2D, _, _)) :: tail => {
          firstPoint = Some(v)
          firstPointSet = true
          Start('cad,"create.Input", InputRequest(7,firstPoint,vector2DGuide))
        }
        case End(v : Vector2D) :: tail => {
          if (firstPoint.isEmpty) {
          firstPoint = Some(v)
          //Get input: Mouse up.
          Start('cad, "create.Input", InputRequest(8,None,vector2DGuide))
          //If mouse up is a different point: A drag occured, and a line was drawn. End.
        } else if (v != firstPoint.get & v.distanceTo(firstPoint.get) > Siigna.selectionDistance & firstPointSet == false) {
          val line = LineShape(firstPoint.get,v).addAttributes(attributes)
          Create(line)
          firstPoint = None
          End
          //Otherwise the mouse was not moved, and first point was set by a click. Request input for second point.
        } else if (firstPointSet == false) {
          firstPointSet = true
          Start('cad,"create.Input", InputRequest(7,firstPoint,vector2DGuide))
        } else {
          //A second point was set. Create the line.
          val line = LineShape(firstPoint.get,v).addAttributes(attributes)
          Create(line)
          firstPoint = None
          End
        }

      }

      //Handle enter, backspace and unexpected events
      case (End(KeyDown(Key.enter,_)) | End(KeyDown(Key.backspace,_))) :: tail => //Do nothing

      //Request input
      case _ => {
        Start('cad, "create.Input", InputRequest(6,None))
      }
    }
  )
}