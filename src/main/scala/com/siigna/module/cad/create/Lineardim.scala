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

/* 2011 (C) Copyright by Siigna, all rights reserved. */

import com.siigna._
import module.{Tooltip, ModuleInit}
import java.awt.Color

class Lineardim extends Module {

  var currentMouse : Option[Vector2D] = None

  val color = "Color" -> new Color(0.25f, 0.25f, 0.25f, 1.00f)
  val colorBlack = "Color" -> new Color(0.00f, 0.00f, 0.00f, 1.00f)
  var dimTextSize = 3
  private var transformation = TransformationMatrix()

  def diaMark(point : Vector2D) = if (hasBothPoints)
      Some(LineShape((diaRotation1.get + point + normalUnitVector2D(points(0),points(1)) * scale * offsetDistance),
                     (diaRotation2.get + point + normalUnitVector2D(points(0),points(1)) * scale * offsetDistance)))
    else
      None
  def diaMark1 = if (hasBothPoints) diaMark(points(0)) else None

  def diaMark2 = if (hasBothPoints) diaMark(points(1)) else None

  def diaRotation(degree : Double) = if (hasBothPoints)
      Some(transformation.rotate(degree).transform(normalUnitVector2D(points(0),points(1)) * (scale)))
    else
      None

  def diaRotation1 = diaRotation(45)
  def diaRotation2 = diaRotation(225)

  def dimText : Option[Shape] =
    if (hasBothPoints) {
      val offset = normalUnitVector2D(points(1),points(0)) * 2
      val tX = points(0).x + (points(1).x - points(0).x)/2 - (offset.x * 3 * scale) - 1.8 * scale
      val tY = points(0).y + (points(1).y - points(0).y)/2 - (offset.y * 2.5 * scale) + 1.6 * scale
      Some(TextShape((points(1)-(points(0))).length.round.toString,Vector2D(tX,tY),scale * dimTextSize))
    }
    else
      None

  def dynamicDimText : Option[Shape] = if (!hasBothPoints)

    Some(TextShape((currentMouse.get-(points(0))).length.round.toString,
                    ((points(0) + normalUnitVector2D(points(0),currentMouse.get)) + (currentMouse.get-points(0))/2),
                      scale))
    else
      None

  var finalOffset : Option[Vector2D] = None

  def hasBothPoints = (points.size >= 2)

  def normalShape(point : Vector2D) = if (hasBothPoints)
      Some(LineShape((point - normalUnitVector2D(points(1),points(0)) * (scale/2 * offsetDistance)),
                     (point + normalUnitVector2D(points(0),points(1)) * scale * offsetDistance)))
    else
      None

  def normalShape1 = if (hasBothPoints) normalShape(points(0)) else None

  def normalShape2 = if (hasBothPoints) normalShape(points(1)) else None

  def normalUnitVector2D(v1 : Vector2D, v2 : Vector2D) = {
    if (offsetSide == true)
      Vector2D((v2.x - v1.x) , (v2.y - v1.y)).normal.unit
    else
      Vector2D((v2.y - v1.y), -(v2.x - v1.x)).unit
  }

  var offsetSide : Boolean = false
  var offsetDistance = 5
  private var points : List[Vector2D] = List()
  private var scale = 1

  def simpleA : Option[Shape] = if (currentMouse.isDefined && points.length > 0)
      Some(LineShape(currentMouse.get,(points(0))))
    else
      None

  def shapeA : Option[Shape] = if (hasBothPoints)
                   //first point
    Some(LineShape((points(1) + normalUnitVector2D(points(0),points(1)) * scale * offsetDistance),
                   //second point: almost half the distance - leaving a gap for the text field.
                   (points(0)+ ((points(1) - points(0))/1.8) + normalUnitVector2D(points(0),points(1)) * scale * offsetDistance)))
    else
      None

  def shapeB : Option[Shape] = if (hasBothPoints)
                   //first point
    Some(LineShape((points(0) + normalUnitVector2D(points(0),points(1)) * scale * offsetDistance),
                   //second point: almost half the distance - leaving a gap for the text field.
                   (points(1)+ ((points(0) - points(1))/1.8) + normalUnitVector2D(points(0),points(1)) * scale * offsetDistance)))
    else
      None



  def stateMap = Map(

    'Start -> {
      //exit mechanisms
      case End(MouseDown(p,MouseButtonRight,modifier)) :: tail => End
      case End(KeyDown(Key.escape,modifier)) :: tail => End

      case End(p : Vector2D) :: tail => {
        points = points :+ p
        if (points.length == 1) {
          val vector2DGuide = DynamicDrawFromVector2D((v: Vector2D) => Traversable(LineShape(p, v)))
          val inputRequest = InputRequest(6,None,vector2DGuide)
          Start('cad,"create.Input", inputRequest)
        } else if (points.length == 2) {
          val line = points(1) - points(0)
          val point = points(0) - p
          val scalar = line.normal * point
          if (scalar >= 0)
            offsetSide = false
          else
            offsetSide = true
          Siigna display "click on the side away from the pointers"
          Tooltip.blockUpdate(3500)

          val vector2DGuide = DynamicDrawFromVector2D((v: Vector2D) => Traversable(LineShape(points(0), points(1))))

          val inputRequest = InputRequest(2,None,vector2DGuide)
          Start('cad,"create.Input", inputRequest)

          //Input 2: Vector2D, only by mouseDown

        } else if (points.length == 3) {
          //Finalise
          val line = points(1) - points(0)
          val point = points(0) - p
          val scalar = line.normal * point
          if (scalar >= 0)
            offsetSide = false
          else
            offsetSide = true
          //Creates a CollectionShape:
          Create(
            shapeA.get.setAttributes(colorBlack, "StrokeWidth" -> 0.09),
            shapeB.get.setAttributes(colorBlack, "StrokeWidth" -> 0.09),
            normalShape1.get.setAttributes(colorBlack, "StrokeWidth" -> 0.09),
            normalShape2.get.setAttributes(colorBlack, "StrokeWidth" -> 0.09),
            diaMark1.get.setAttributes(color, "StrokeWidth" -> 0.7),
            diaMark2.get.setAttributes(color, "StrokeWidth" -> 0.7),
            dimText.get.setAttribute(colorBlack)
          )
         End
        }
      }
      
      

      case _ => {
        //change cursor to crosshair
        Siigna.setCursor(Cursors.crosshair)

      //get the current paperScale
      scale = Siigna.paperScale
        Tooltip.updateTooltip(List("Linear Dimension tool active"))
        Start('cad, "create.Input", InputRequest(6,None))
      }
      
      
  })

}