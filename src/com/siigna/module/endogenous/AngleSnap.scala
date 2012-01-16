package com.siigna.module.endogenous

import com.siigna._
import com.siigna.app.view.event.EventSnap

/**
 * A radian snap module that snaps to the point and the radian given.
 */
case class AngleSnap(p: Vector2D, radian : Double) extends EventSnap{

  // Define a line segment that we can perform calculations on
  private val line = Line2D(p, Vector2D(math.cos(radian), math.sin(radian)))

  /**
   * Parse events to follow the guide given in the constructor.
   */
  def parse(event : Event, model : Iterable[ImmutableShape]) = event match {
    case MouseMove(point, a, b) => MouseMove(snapToRadian(point), a, b)
    case MouseDrag(point, a, b) => MouseMove(snapToRadian(point), a, b)
    case MouseDown(point, a, b) => MouseDown(snapToRadian(point), a, b)
    case MouseUp(point, a, b)   => MouseDown(snapToRadian(point), a, b)
    case some => some
  }

  /**
   * Snaps a given point to the guide given in the constructor.
   */
  def snapToRadian(point : Vector2D) : Vector2D = {
    val newPoint = line.closestPoint(point)
    newPoint
  }


}