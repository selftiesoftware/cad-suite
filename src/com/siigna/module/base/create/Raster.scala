package com.siigna.module.base.create

/* 2012 (C) Copyright by Siigna, all rights reserved. */

import java.awt.Color
import com.siigna._

object Raster extends Module {

  //raster color value
  lazy val anthracite  = new Color(0.25f, 0.25f, 0.25f, 1.00f)

  //save the first point and used it to close the fill area.
  private var firstPoint : Option[Vector2D] = None

  //current position of the mouse
  private var mousePosition : Option[Vector2D] = None

  // The points of the raster
  private var points = List[Vector2D]()

  private var previousPoint : Option[Vector2D] = None

  // The closed, filled polyline so far
  private var shape : PolylineShape = PolylineShape.empty

  val eventHandler = EventHandler(stateMap, stateMachine)

  def stateMap = DirectedGraph(
    'Start        -> 'KeyEscape  -> 'End
  )

  def stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      events match {
        case MouseUp(_, MouseButtonRight, _) :: tail => Goto('End)
        case MouseMove(point, _, _) :: tail => mousePosition = Some(point)
        case MouseDrag(point, _, _) :: tail => mousePosition = Some(point)
        case MouseDown(point, MouseButtonLeft, _):: tail => {
          if (firstPoint.isEmpty)
            firstPoint = Some(point)

          points = points :+ point
          previousPoint = Some(point)

          // Set shape
          if (points.size > 0) {
            shape = PolylineShape.fromPoints(points).setAttribute(("raster" -> anthracite))
          }
        }
        case _ =>
      }
    }),
    'End -> ((events : List[Event]) => {

      //add a line segment from the last to the first point in the list to close the fill area
      if (shape.shapes.size > 0) Create(shape)

      //Clear the variables
      firstPoint = None
      points = List[Vector2D]()
      previousPoint = None
      shape = PolylineShape.empty
    })
  )

  override def paint(g : Graphics, t : TransformationMatrix) {
    g draw shape.transform(t)
  }
}