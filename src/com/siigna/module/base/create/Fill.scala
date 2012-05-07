/*
 * Copyright (c) 2012. Siigna is released under the creative common license by-nc-sa. You are free
 * to Share — to copy, distribute and transmit the work,
 * to Remix — to adapt the work
 *
 * Under the following conditions:
 * Attribution —  You must attribute the work to http://siigna.com in the manner specified by the author or licensor (but not in any way that suggests that they endorse you or your use of the work).
 * Noncommercial — You may not use this work for commercial purposes.
 * Share Alike — If you alter, transform, or build upon this work, you may distribute the resulting work only under the same or similar license to this one.
 */

package com.siigna.module.base.create

import java.awt.Color
import com.siigna._

object Fill extends Module {

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
          if (!firstPoint.isEmpty) {
            firstPoint = Some(point)
            points = points :+ point
            previousPoint = Some(point)
        }
          else firstPoint = Some(point)

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
      if (shape.shapes.size > 0) Create(shape.asInstanceOf[Shape])

      //Clear the variables
      firstPoint = None
      points = List[Vector2D]()
      previousPoint = None
      shape = PolylineShape.empty
    })
  )

  override def paint(g : Graphics, t : TransformationMatrix) {
    if(!firstPoint.isEmpty && mousePosition.isDefined && previousPoint.isDefined && points.length > 0) {
      g draw shape.transform(t)
    }
  }
}