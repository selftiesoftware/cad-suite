package com.siigna.module.endogenous.create

import com.siigna._
import com.siigna.app.view.event.EventSnap

/**
 * Created by IntelliJ IDEA.
 * User: oep
 * Date: 15-01-12
 * Time: 12:23
 * To change this template use File | Settings | File Templates.
 */

class RadianSnap(p: Vector2D, radian : Double) extends EventSnap{

  def parse(event : Event, model : Iterable[ImmutableShape]) = event match {
    case MouseMove(point, a, b) => MouseMove(snapToRadian(point), a, b)
    case some => {
      some
    }
    //transforming events with the snapToRadian function
  }

  //function that takes a point and returns another point
  def snapToRadian(point : Vector2D) : Vector2D = {
    var mouseX = point.x
    var mouseY = point.y
    point
  }


}