package com.siigna.module.base.create

/* 2011 (C) Copyright by Siigna, all rights reserved. */

import com.siigna._
import com.siigna.module.base.Default


//TODO: clean up this mess!!

object Lineardim extends Module {

  var currentMouse : Option[Vector2D] = None

  val color = "Color" -> "#AAAAAA".color

  def diaMark(point : Vector2D) = if (hasBothPoints)
      Some(LineShape((diaRotation1.get + point + normalUnitVector2D(points(1),points(2)) * scale) , (diaRotation2.get + point + normalUnitVector2D(points(1),points(2)) * scale)).addAttribute(color))
    else
      None
  def diaMark1 = if (hasBothPoints) diaMark(points(1)) else None

  def diaMark2 = if (hasBothPoints) diaMark(points(2)) else None

  def diaRotation(degree : Double) = if (hasBothPoints)
      Some(transformation.rotate(degree).transform(normalUnitVector2D(points(1),points(2)) * (scale/4)))
    else
      None
  def diaRotation1 = diaRotation(45)

  def diaRotation2 = diaRotation(225)

  def dimText : Option[Shape] = if (hasBothPoints)
      Some(TextShape((points(2)-(points(1))).length.toInt.toString , ((points(1) + normalUnitVector2D(points(1),points(2)) * scale*2.5) + (points(2)-points(1))/2) , scale, Attributes("AdjustToScale" -> true)))
    else
      None

  def dynamicDimText : Option[Shape] = if (!hasBothPoints)
      Some(TextShape((currentMouse.get-(points(1))).length.toInt.toString , ((points(1) + normalUnitVector2D(points(1),currentMouse.get) * scale*2.5) + (currentMouse.get-points(1))/2) , scale, Attributes("AdjustToScale" -> true)))
    else
      None

  val eventHandler = EventHandler(stateMap, stateMachine)

  var finalOffset : Option[Vector2D] = None

  def hasBothPoints = (points.size >= 3)

  def normalShape(point : Vector2D) = if (hasBothPoints)
      Some(LineShape((point - normalUnitVector2D(points(2),points(1)) * (scale/2)) , (point + normalUnitVector2D(points(1),points(2)) * (scale*1.3))).addAttributes(color))
    else
      None

  def normalShape1 = if (hasBothPoints) normalShape(points(1)) else None

  def normalShape2 = if (hasBothPoints) normalShape(points(2)) else None

  def normalUnitVector2D(v1 : Vector2D, v2 : Vector2D) = {
    if (offsetSide == true)
      Vector2D((v2.x - v1.x) , (v2.y - v1.y)).normal.unit
    else
      Vector2D((v2.y - v1.y), -(v2.x - v1.x)).unit // Normal*3 = Vector2D(y, -x)
  }
  var offsetSide : Boolean = false

  def dynamicA : Option[Shape] = if (currentMouse.isDefined)
      Some(LineShape((currentMouse.get + normalUnitVector2D(points(1),currentMouse.get) * scale) , (points(1) + normalUnitVector2D(points(1),currentMouse.get) * scale)).addAttributes(color))
    else
      None

  def simpleA : Option[Shape] = if (currentMouse.isDefined && points.size > 0)
      Some(LineShape(currentMouse.get,(points(1))).addAttributes(color))
    else
      None

  def shapeA : Option[Shape] = if (hasBothPoints)
      Some(LineShape((points(2) + normalUnitVector2D(points(1),points(2)) * scale) , (points(1) + normalUnitVector2D(points(1),points(2)) * scale)).addAttributes(color))
    else
      None

  var transformation = TransformationMatrix()
  var norm = Vector2D(0,0)
  var points : List[Vector2D] = List()
  var previousPoint : Option[Vector2D] = None
  var scale = com.siigna.app.model.Model.boundaryScale * 5

  def stateMap = DirectedGraph(

    'Start         -> 'KeyEscape -> 'End,
    'SelectSide    -> 'KeyEscape -> 'End

  )

  def stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      events match {
        //set the first point of the dim line
        case MouseDown(p, _, _):: tail => {
          points = List(p)
          Goto('SecondPoint)
        }
        case _ =>
      }
      None
      }),
    'SecondPoint -> ((events : List[Event]) => {
      events match {
        //draw the other end dynamically
        case MouseMove(p, _, _):: tail => {
          currentMouse = Some(p)
        }
        //set the other end of the dim line
        case MouseDown(p, _, _):: tail => {

            points = points :+ p

        }
        case MouseUp(_, _, _)::tail => {
          if (points.size == 3)
            Goto('SelectSide)
        }
        case _ =>
      }
      None
      }),
    'SelectSide -> ((events : List[Event]) => {
      events match {
        //point the mouse to the side the dim line should be offset
        case MouseMove(p, _, _):: tail => {
          val line = points(2) - points(1)
          val point = points(1) - p
          val scalar = line.normal * point
          if (scalar >= 0)
            offsetSide = false
          else
            offsetSide = true
        finalOffset = Some(p)

        }
        case MouseDown(_, MouseButtonRight, _):: tail => Goto('End)
        case MouseDown(_, MouseButtonLeft, _):: tail => {
        //finalOffset = Some(p)
          Goto('End)
        }
        case _ =>
      }
    None
    }),
    'End -> ((events : List[Event]) => {
      events match {
        case _ =>
          val line = points(2) - points(1)
          val point = points(1) - finalOffset.get
          val scalar = line.normal * point
          if (scalar >= 0)
            offsetSide = false
          else
            offsetSide = true
          Create(shapeA.get,normalShape1.get,normalShape2.get,diaMark1.get,diaMark2.get,dimText.get)
          //clear the list of points
          points = List[Vector2D]()
      Default.previousModule = Some('Lineardim)
      }

    })
  )
  override def paint(g : Graphics, t : TransformationMatrix) {
    if (currentMouse.isDefined && !hasBothPoints && simpleA.isDefined && dynamicDimText.isDefined) {
      g draw simpleA.get.transform(t)
      g draw dynamicDimText.get.transform(t)
    }
    if (currentMouse.isDefined && hasBothPoints) {
      g draw dynamicA.get.transform(t)
      g draw normalShape1.get.transform(t)
      g draw normalShape2.get.transform(t)
      g draw diaMark1.get.transform(t)
      g draw diaMark2.get.transform(t)
      g draw dimText.get.transform(t)
    }
  }
}