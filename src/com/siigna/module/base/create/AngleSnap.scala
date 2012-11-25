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

import com.siigna._
import com.siigna.util.event.EventSnap
import collection.parallel.immutable.{ParMap, ParIterable}

/**
 * A radian snap module that snaps to the point and the radian given.
 *
 * @param center  The center point of the angle snap
 * @param degree The degree to snap to, calculated clockwise from North
 */
case class AngleSnap(center: Vector2D, degree : Int) extends EventSnap{

  /**
   * Hack to make sure we use radians to calculate the correct angle.
   * Radians are defined counter-clockwise from East
   */
  private val radian = (degree * -1 + 90) * math.Pi / 180

  // Define a line segment that we can perform calculations on
  private val line = Line2D(center, center + Vector2D(math.cos(radian), math.sin(radian)))

  /**
   * Parse events to follow the guide given in the constructor.
   */
  def parse(event : Event, model : Map[Int, Shape]) = event match {
    case MouseMove(point, a, b) => MouseMove(snapToRadian(point), a, b)
    case MouseDrag(point, a, b) => MouseMove(snapToRadian(point), a, b)
    case MouseDown(point, a, b) => MouseDown(snapToRadian(point), a, b)
    case MouseUp(point, a, b)   => MouseUp(snapToRadian(point), a, b)
    case some => some
  }

  /**
   * Snaps a given point to the guide given in the constructor.
   */
  def snapToRadian(point : Vector2D) : Vector2D = line.closestPoint(point)


}