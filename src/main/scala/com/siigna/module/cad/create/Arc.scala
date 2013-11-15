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

/* 2012 (C) Copyright by Siigna, all rights reserved. */

import com.siigna._
import app.Siigna
import java.awt.Color
import module.Tooltip

class Arc extends Module {

  var r = TransformationMatrix()

  var firstPoint : Option[Vector2D] = None
  var secondPoint : Option[Vector2D] = None

  val attributes = {
    val color = Siigna.color("activeColor")
    val lineWidth = Siigna.double("activeLineWidth")
    Attributes(Seq(color.map(c => "Color" -> color.getOrElse(None)), lineWidth.map(w => "StrokeWidth" -> lineWidth.getOrElse(None))).flatten)
  }

  private val vector2DLineGuide = DynamicDrawFromVector2D((v : Vector2D) => Traversable(LineShape(firstPoint.get,v).addAttributes(attributes)))

  private val vector2DArcGuide = DynamicDrawFromVector2D((v : Vector2D) => {
    try{Traversable(ArcShape(firstPoint.get,v,secondPoint.get).addAttributes(attributes))
    } catch {
      case _ : Throwable => Traversable.empty
    }
  })

  val doubleGuide = DynamicDrawFromDouble((d : Double) => Traversable(ArcShape(firstPoint.get,middlePointForDoubleGuide(d),secondPoint.get).addAttributes(attributes)))

  def middlePointForDoubleGuide (d: Double) : Vector2D  = {
    val nv: Vector2D = Vector2D(-(secondPoint.get.y - firstPoint.get.y)/2,(secondPoint.get.x - firstPoint.get.x)/2)
    val v: Vector2D = Vector2D((secondPoint.get.x - firstPoint.get.x)/2,(secondPoint.get.y - firstPoint.get.y)/2)
    val l: Double = nv.length
    val sv: Vector2D = firstPoint.get + v + Vector2D((nv.x/l)*d,(nv.y/l)*d)
    sv
  }

  def stateMap = Map(
    'Start -> {
      case _ => {
        //change cursor to crosshair
        Siigna.setCursor(Cursors.crosshair)
        //Update tooltip
        Tooltip.updateTooltip(List("Arc tool active","",""))
        //Goto next state
        'ReceiveFirstPoint
      }
    },
    'ReceiveFirstPoint -> {
      //Exit strategy
      case (End | KeyDown(Key.Esc, _) | End(KeyDown(Key.escape, _)) | MouseDown(_, MouseButtonRight, _) | End(MouseDown(_,MouseButtonRight, _)) ) :: tail => End

      //Handle values returned from input
      case End(v : Vector2D) :: tail => {
        firstPoint = Some(v)
        'receiveSecondPoint
      }

      //Handle enter, backspace and unexpected events
      case (End(KeyDown(Key.enter,_)) | End(KeyDown(Key.backspace,_))) :: tail =>
      case _ => {
        //Request input
        Start('cad, "create.Input", InputRequest(6,None))
      }
    },

    'receiveSecondPoint -> {
      //Exit strategy
      case (End | KeyDown(Key.Esc, _) | End(KeyDown(Key.escape, _)) | MouseDown(_, MouseButtonRight, _) | End(MouseDown(_,MouseButtonRight, _)) ) :: tail => End

      //Handle values returned from input
      case End(v : Vector2D) :: tail => {
        if (v != firstPoint.get) {
          secondPoint = Some(v)
          'receiveThirdPointOrRadius
        } else Start('cad,"create.Input", InputRequest(7,firstPoint,vector2DLineGuide))
      }

      //Handle enter, backspace and unexpected events
      case End(KeyDown(Key.enter,modifier)) :: tail =>
      case End(KeyDown(Key.backspace,modifier)) :: tail => {
        firstPoint = None
        'ReceiveFirstPoint
      }
      case _ => {
        //Request input
        Start('cad,"create.Input", InputRequest(7,firstPoint,vector2DLineGuide))
      }
    },

    'receiveThirdPointOrRadius -> {
      //Exit strategy
      case (End | KeyDown(Key.Esc, _) | End(KeyDown(Key.escape, _)) | MouseDown(_, MouseButtonRight, _) |End(MouseDown(_,MouseButtonRight, _)) ) :: tail => End

      //Handle values returned from input
      //A point is received
      case End(v : Vector2D) :: tail => {
        // If the received point is not the same as start or endpoint, and
        // the three points are not on a straight line, then an arc shape is created and module closes.
        if ((v != firstPoint.get) && (v != secondPoint.get) && (math.abs((firstPoint.get.x - v.x)/(firstPoint.get.y - v.y) ) != math.abs((secondPoint.get.x - v.x)/(secondPoint.get.y - v.y)))) {
          val arc = ArcShape(firstPoint.get,v,secondPoint.get).addAttributes(attributes)
          Create(arc)
          End
        } else Start('cad,"create.Input", InputRequest(9,firstPoint,vector2DArcGuide, doubleGuide))
      }
      //A radius is received: Create the arc
      case End(d : Double) :: tail => {
        val arc = ArcShape(firstPoint.get,middlePointForDoubleGuide(d),secondPoint.get).addAttributes(attributes)
        Create(arc)
        End
      }

      //Handle enter, backspace and unexpected events
      case End(KeyDown(Key.enter,modifier)) :: tail =>
      case End(KeyDown(Key.backspace,modifier)) :: tail => {
        secondPoint = None
        'receiveSecondPoint
      }
      case _ => {
        //Request input
        Start('cad, "create.Input", InputRequest(9,firstPoint,vector2DArcGuide, doubleGuide))

      }
    }
  )
}
