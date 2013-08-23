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

class Circle extends Module {

  val c = CircleShape(Vector2D(0, 0), 50)
  var center : Option[Vector2D] = None
  var r = TransformationMatrix()

  val attributes = {
    val color = Siigna.color("activeColor")
    val lineWidth = Siigna.double("activeLineWidth")
    Attributes(Seq(color.map(c => "Color" -> color.getOrElse(None)), lineWidth.map(w => "StrokeWidth" -> lineWidth.getOrElse(None))).flatten)
  }

  def stateMap = Map(
    //StartCategory: Defines a centerpoint for the circle and forwards to 'SetRadius
    'Start -> {
      case events => {
        events match {
          case End(MouseDown(p,MouseButtonRight,modifier)) :: tail => End
          case End(KeyDown(Key.escape,modifier)) :: tail => End

          //If a point is entered, it is the centre:
          case End(p : Vector2D) :: tail => {
            center = Some(p)
            //Send guides, and ask for one-coordinate input: Radius - from point by click, or by key-entry.
            val doubleGuide = DoubleGuideNew((r: Double) => Traversable(CircleShape(p, math.abs(r)).addAttributes(attributes)))
            val vector2DGuide = Vector2DGuideNew((p: Vector2D) => Traversable(CircleShape(center.get, math.sqrt(( (center.get.x-p.x) * (center.get.x-p.x)) + ( (center.get.y-p.y) * (center.get.y-p.y)) )).addAttributes(attributes)))
            val inputRequest = InputRequestNew(10,center,vector2DGuide, doubleGuide)
            Start('cad,"create.InputNew", inputRequest)

            //val inputRequest = InputRequest(Some(vector2DGuide),Some(doubleGuide),None,None,None,None,center,None,None,Some(3))
            //Start('cad, "create.Input", inputRequest)
          }

          case End(r : Double) :: tail => {
            val circle = CircleShape(center.get, math.abs(r)).addAttributes(attributes)
              Create(circle)

            End
          }

          case End :: tail => End

          //Starts point, asks for two-value-input (center):
          case _ => {
            Start('cad, "create.InputNew", InputRequestNew(6,None))
          }
        }
      }
    }
  )

}