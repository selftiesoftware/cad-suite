/* 2009 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

import java.awt.Color

import com.siigna._
import app.controller.ModuleBank

object Point extends Module {

  var isGizmoCheckNeeded = false

  val eventHandler = EventHandler(stateMap, stateMachine)
  var mousePosition : Option[Vector] = None
  var previousPoint : Option[Vector] = None
  var pointGuide : Option[PointGuide] = None

  var shape : Option[Shape] = None

  var coordinateValue : String = ""
  var coordinateX : Option[Double] = None
  var coordinateY : Option[Double] = None

  def difference : Vector = if (previousPoint.isDefined) previousPoint.get else Vector(0, 0)

  /**
   * Clear variables
   */
  def clearVariables : Unit = {
    mousePosition = None
    previousPoint = None
    pointGuide = None
    shape = None
    coordinateValue = ""
    coordinateX = None
    coordinateY = None
  }

  // Save the X value, if any
  def x : Option[Double] = if (!coordinateX.isEmpty)
      coordinateX
    else if (coordinateValue.length > 0 && coordinateValue != "-")
      Some(java.lang.Double.parseDouble(coordinateValue) + difference.x)
    else if (coordinateX.isDefined)
      Some(coordinateX.get + difference.x)
    else None

  // Save the Y value, if any
  def y : Option[Double] = if (coordinateY.isDefined)
      coordinateY
    else if (coordinateX.isDefined && coordinateValue.length > 0 && coordinateValue != "-")
      Some(java.lang.Double.parseDouble(coordinateValue) + difference.y)
    else if (coordinateY.isDefined)
      Some(coordinateY.get + difference.y)
    else None

  def stateMap = DirectedGraph(
    'Start         -> 'Action    -> 'End,
    'Start         -> 'MouseUp   -> 'End
  )

  def stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      events match {
        case (g : PointGuide) :: tail => {
          pointGuide = Some(g)
        }
        case Message(shape : Shape) :: tail => {
            shape match {
              case LineShape(p1, p2, _) => previousPoint = Some(p2)
              case ArcShape(p1, p2, p3, _) => previousPoint = Some(p3)
              case PointShape(p, _) => previousPoint = Some(p)
              case _ =>
            }
            this.shape = Some(shape)
        }
        case _ :: MouseMove(point, _, _) :: tail => {
            if (previousPoint.isDefined)
            mousePosition = Some(point)
        }
        case _ :: MouseDrag(point, _, _) :: tail => mousePosition = Some(point)
        case _ =>
      }

      events match {
        case MouseMove(point,_,_) :: tail => mousePosition = Some(point)
        case KeyDown(Key.Backspace, _) :: tail => {
          if (coordinateValue.length > 0) coordinateValue = coordinateValue.substring(0, coordinateValue.length-1)
          else if (coordinateX.isDefined) {
            coordinateValue = coordinateX.get.toString
            coordinateX     = None
          }
        }
        case KeyDown(Key.Enter | Key.Tab | ',', _) :: tail => {
          if (coordinateX.isEmpty && coordinateValue.length == 0) Goto('End)
          if (coordinateX.isEmpty && coordinateValue.length > 0) {
            coordinateX = Some(java.lang.Double.parseDouble(coordinateValue))
            coordinateValue = ""
          } else if (coordinateY.isEmpty && coordinateValue.length > 0) {
            coordinateY = Some(java.lang.Double.parseDouble(coordinateValue))
            coordinateValue = ""
            Goto('End)
          }
        }
        case KeyDown(Key.Space, _) :: tail => Goto('End)
        case KeyDown(code, _) :: tail => {
          val char = code.toChar
          if (char.isDigit)
            coordinateValue += char
          else if ((char == '.') && !coordinateValue.contains('.'))
            coordinateValue += "."
          else if (char == '-' && coordinateValue.length < 1)
            coordinateValue = "-"
        }
        case MouseDown(pointDown, _, _) :: tail => {
          if (isGizmoCheckNeeded) {
            Preload('AngleGizmo, "com.siigna.module.endogenous.point.AngleGizmo")
            ForwardTo('AngleGizmo)
            isGizmoCheckNeeded = false
          } else {
            isGizmoCheckNeeded = true
            Goto('End)
          }

        }
        case _ =>
      }

      if (Model.isEmpty && previousPoint.isEmpty && shape.isEmpty)
        interface display "Click to define starting point."
      else {
        if (coordinateValue.length > 0) {
          val x = if (coordinateX.isDefined) "%.3f" format coordinateX.get
                  else coordinateValue
          val y = if (coordinateY.isDefined) "%.3f" format coordinateY.get
                  else if (coordinateX.isDefined) coordinateValue
                  else ""
          interface display "Enter coordinate (X: "+x+", Y: "+y+")."
        } else if (mousePosition.isDefined) {
          val x = "%.3f" format (if (coordinateX.isDefined) coordinateX.get else mousePosition.get.x) - difference.x
          val y = "%.3f" format mousePosition.get.y - difference.y
          interface display "Click to define coordinate (X: "+x+", Y: "+y+")."
        } else
          interface display "Enter (X, Y) or click to define coordinate."
      }
    }),
    'End -> ((events : List[Event]) => {
      def setPoint(point : Vector) = {
        // Define the result
        val result = if (x.isDefined && y.isDefined)
          Create(PointShape(Vector(x.get, y.get)))
        else if (x.isDefined)
          Create(PointShape(Vector(x.get, point.y)))
        else
          Create(PointShape(point))
        // clear everything up
        clearVariables
        // return
        result
      }
      events match {
        case action : CreateShape => clearVariables; Some(action)
        case MouseDown(_, MouseButtonRight, _) :: tail => clearVariables; None
        case MouseUp(_, MouseButtonRight, _) :: tail => clearVariables; None
        case MouseDown(point, _, _) :: tail => setPoint(point)
        case MouseUp(point, _, _) :: tail => setPoint(point)
        case _ => {
          if (coordinateX.isDefined && coordinateY.isDefined) {
            val x = coordinateX.get
            val y = coordinateY.get
            clearVariables
            Message(PointShape(Vector(x, y) + difference))
          } else {
            clearVariables
          }
        }
      }
    })
  )

  override def paint(g : Graphics, t : TransformationMatrix) {
    // Draw the crosshair
    if (x.isDefined) {
      val color = new Color(0.2f, 0.2f, 0.2f, 0.65f)
      val xToVirtual = Vector(x.get, 0).transform(t).x
      val screenBottomRight = Siigna.screen.bottomRight
      val screenTopLeft     = Siigna.screen.topLeft
      g draw LineShape(Vector(xToVirtual, screenTopLeft.y), Vector(xToVirtual, screenBottomRight.y)).attributes_+=("Color" -> color)
      if (y.isDefined) {
        val yToVirtual = Vector(0, y.get).transform(t).y
        g draw LineShape(Vector(screenTopLeft.x, yToVirtual), Vector(screenBottomRight.x, yToVirtual)).attributes_+=("Color" -> color)
        g draw LineShape(Vector(xToVirtual, yToVirtual), Vector(xToVirtual, yToVirtual)).transform(t)
      }
    }

    // Draw a point guide with the new point as a parameter
    val guide : Vector => ImmutableShape = if (pointGuide.isDefined)
      pointGuide.get.guide
    else if (previousPoint.isDefined)
      LineShape(previousPoint.get, _)
    else PointShape(_)

    if (x.isDefined && y.isDefined)
      g draw guide(Vector(x.get, y.get)).transform(t)
    else if (x.isDefined && mousePosition.isDefined)
      g draw guide(Vector(x.get, mousePosition.get.y)).transform(t)
    else if (mousePosition.isDefined)
      g draw guide(mousePosition.get).transform(t)
  }

}

/**
 * A class used to draw guides in the point module.
 */
case class PointGuide(guide : Vector => ImmutableShape)