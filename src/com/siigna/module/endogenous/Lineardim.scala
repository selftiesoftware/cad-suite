/* 2011 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

import com.siigna._

/**
 * TODO: points => p1 and p2
 */
class Lineardim extends Module {

  val color = "Color" -> "#AAAAAA".color

  def diaMark(point : Vector) = if (hasBothPoints)
      Some(LineShape((diaRotation1.get + point + normalUnitVector(points(0),points(1)) * scale) , (diaRotation2.get + point + normalUnitVector(points(0),points(1)) * scale)).attributes_+=(color))
    else
      None

  def diaMark1 = if (hasBothPoints) diaMark(points(0)) else None
  def diaMark2 = if (hasBothPoints) diaMark(points(1)) else None

  def diaRotation(degree : Double) = if (hasBothPoints)
      Some(transformation.rotate(degree).transform(normalUnitVector(points(0),points(1)) * (scale/4)))
    else
      None

  def diaRotation1 = diaRotation(45)
  def diaRotation2 = diaRotation(225)

  def dimText : Option[Shape] = if (hasBothPoints)
      Some(TextShape((points(1)-(points(0))).length.toInt.toString , ((points(0) + normalUnitVector(points(0),points(1)) * scale*2.5) + (points(1)-points(0))/2) , scale, Attributes("AdjustToScale" -> true)))
    else
      None

  val eventHandler = EventHandler(stateMap, stateMachine)

  def hasBothPoints = (points.size >= 2)

  def normalShape(point : Vector) = if (hasBothPoints)
      Some(LineShape((point + normalUnitVector(points(0),points(1)) * (scale*1.1)) , (point + normalUnitVector(points(0),points(1)) * (scale/2))).attributes_+=(color))
    else
      None

  def normalShape1 = if (hasBothPoints) normalShape(points(0)) else None
  def normalShape2 = if (hasBothPoints) normalShape(points(1)) else None

  def normalUnitVector(v1 : Vector, v2 : Vector) = {
    if (offsetSide == true)
      Vector((v2.x - v1.x) , (v2.y - v1.y)).normal.unit
    else
      Vector((v2.y - v1.y), -(v2.x - v1.x)).unit // Normal*3 = Vector(y, -x)
  }

  var offsetSide : Boolean = false

  def shapeA : Option[Shape] = if (hasBothPoints)
      Some(LineShape((points(1) + normalUnitVector(points(0),points(1)) * scale) , (points(0) + normalUnitVector(points(0),points(1)) * scale)).attributes_+=(color))
    else
      None

  var transformation = TransformationMatrix()

  var norm = Vector(0, 0)

  var points : List[Vector]   = List()

  var previousPoint : Option[Vector] = None

  var scale = com.siigna.app.model.Model.boundaryScale * 5

  override def paint(g : Graphics, t : TransformationMatrix) {
    if (hasBothPoints) {
      g draw shapeA.get.transform(t)
      g draw normalShape1.get.transform(t)
      g draw normalShape2.get.transform(t)
      g draw diaMark1.get.transform(t)
      g draw diaMark2.get.transform(t)
      g draw dimText.get.transform(t)
    }
  }

  def stateMap = DirectedGraph(
    'Start         -> 'MouseDown -> 'CreatingPoint,
    'Start         -> 'MouseMove -> 'Start,
    'Start         -> 'KeyEscape -> 'End,
    'Start         -> 'Action    -> 'FirstPoint,
    'FirstPoint    -> 'Action    -> 'SecondPoint,
    'FirstPoint    -> 'KeyEscape -> 'End,
    'SecondPoint   -> 'MouseUp   -> 'SelectSide,
    'SelectSide    -> 'Action    -> 'End,
    'SelectSide    -> 'KeyEscape -> 'End,
    'SelectSide    -> 'MouseDown -> 'End
  )

  def stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      if (previousPoint.isEmpty)
        ForwardTo('Point)
      else {
        points = List(previousPoint.get)
        Goto('FirstPoint)
      }
      None
    }),
    'FirstPoint -> ((events : List[Event]) => {
      events match {
        case Message(p : PointShape) :: tail => {
          points = List(p.point)
        }
        case _ =>
      }
      ForwardTo('Point)
    }),
    'SecondPoint -> ((events : List[Event]) => {
      events match {
        case Message(p : PointShape) :: tail => {
          points = points.::(p.point)
         // TODO: hvordan roteres text???
        }
        case _ =>
      }
    }),
    'SelectSide -> ((events : List[Event]) => {
      events match {
        case MouseMove(p, _, _) :: tail => {
          if (hasBothPoints) {
            val line = points(1) - points(0)
            val point = points(0) - p
            val scalar = line.normal * point
            if (scalar >= 0)
              offsetSide = false
            else
              offsetSide = true
          }
        }
        case _ =>
      }
    }),
    'End -> ((events : List[Event]) => {
      events match {
        case MouseDown(point, _, _) :: tail => {
          Create(shapeA.get,normalShape1.get,normalShape2.get,diaMark1.get,diaMark2.get,dimText.get)
        }
        case _ =>
      }
    })
  )

}