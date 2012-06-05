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

package com.siigna.module.base.helpers

/* 2012 (C) Copyright by Siigna, all rights reserved. */

import com.siigna._

object Area extends Module {

  //a function to calculate an area defined by points.
  def area(points : List[Vector2D]) : Int = {

    var area : Double = 0
    var i : Int = 0

    //TODO: Add the first element to the end to close the polygon

    while (i < points.length - 1) {
      var pointX1 = points(i).x
      var pointY1 = points(i).y

      var pointX2 = points(i+1).x
      var pointY2 = points(i+1).y

      //area += (x.Items.Item(i) * y.Items.Item(i + 1) - x.Items.Item(i + 1) * y.Items.Item(i))
      area += pointX1 * pointY2 - pointX2 * pointY1
      i += 1
    }
    area = scala.math.abs(area * 0.5)
    area.toInt
  }

  //save the first point and used it to close the fill area.
  private var firstPoint : Option[Vector2D] = None

  // The points of the raster
  private var points = List[Vector2D]()

  private var previousPoint : Option[Vector2D] = None
  //a flag used to bypass the first MouseDown event coming from Default when the module is called
  private var setFirstPoint = false

  // The closed, filled polyline so far
  private var shape : PolylineShape = PolylineShape.empty

  val eventHandler = EventHandler(stateMap, stateMachine)

  def stateMap = DirectedGraph(
    'Start        -> 'KeyEscape  -> 'End
  )

  def stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      events match {
        case MouseUp(_, MouseButtonRight, _) :: tail => {
          Goto('End)
        }
        case MouseDown(point, MouseButtonLeft, _):: tail => {
          //mechanism to bypass the MouseDown coming from Default
          if(setFirstPoint == false) {
            setFirstPoint = true
            Goto('Start, false)
          }
          else if(!firstPoint.isDefined) {
            firstPoint = Some(point)
            points = points :+ point
            previousPoint = Some(point)
          }
          else {
            points = points :+ point
            previousPoint = Some(point)
          }
        }
        case MouseUp(_, MouseButtonRight, _):: tail => {
          Goto ('End)
        }
        case _ =>
      }
    if (points.size > 0)

    shape = PolylineShape(points)
    }),
    'End -> ((events : List[Event]) => {

      //add a line segment from the last to the first point in the list to close the fill area
      if (points.size > 0 && firstPoint.isDefined) {
        points = points :+ firstPoint.get

        Siigna.display("Area: "+area(points))

        //TODO: add a textShape object displaying the area if measurement is successful.
        //Create(shape)
      }

      //Clear the variables
      setFirstPoint = false
      firstPoint = None
      points = List[Vector2D]()
      previousPoint = None
      shape = PolylineShape.empty
    })
  )
  override def paint(g : Graphics, t : TransformationMatrix) {
    if (points.length > 0 && previousPoint.isDefined) {

      g draw LineShape(mousePosition, previousPoint.get).transform(t)
      g draw LineShape(mousePosition, firstPoint.get).transform(t)

    } else None
    g draw shape.transform(t)
  }
}