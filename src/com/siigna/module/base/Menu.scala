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

package com.siigna.module.base

import com.siigna.app.view.Graphics
import com.siigna._
import com.siigna.module.base.radialmenu._

import category.{CreateCategory => MenuCreate, StartCategory}

/**
 * The menu module. This module shows the menu as radial items and categories in 13 places (directions):
 * N, NNE, ENE, E, ESE, SSE, S, SSW, WSW, W, WNW and NNW. There is also a center (C) place.
 */
object Menu extends Module {

  var center: Option[Vector2D] = None

  // The current active category
  var currentCategory : MenuCategory = StartCategory

  def stateMap = Map(
    'Start -> {
      case e => {
        'Interaction
      }
    },
    'Interaction -> {
      case MouseDown(_, MouseButtonRight, _) :: tail => {

        'Kill
      }
      case MouseDown(p,_,_) :: tail => {
        println(p, View.center.distanceTo(p))

        if(View.center.distanceTo(p) > 100 && View.center.distanceTo(p) < 150) {
          println("here")

          currentCategory.graph.get(direction(p)) foreach(_ match {
            case mc: MenuCategory => currentCategory = mc

            case MenuModule(instance, icon) => instance
          })
        }
      }
    },

    'Kill -> {
      case _ =>
    }
  )

  /**
   * Paints the menu. If the menu is being initalized we multiply the transformationMatrix with a <code>distanceScale</code> to show an animation
   * of the icons, origining from the center.
   */
  override def paint(g : Graphics, transformation : TransformationMatrix) {

    val location = TransformationMatrix(View.center,1)

    def drawElement(event: MenuEvent, element: MenuElement) {

      val t = TransformationMatrix(event.vector*130,1).concatenate(location)

     /* event.icon.foreach(s => {

        g.draw(s)

      })   */

      // Draw the icons - if the event is the Center we should only transform to the location
      event match {
        case EventC => element.icon.foreach(s => g.draw(s.transform(location)))
        case _ => element.icon.foreach(s => g.draw(s.transform(t)))
      }
    }

    currentCategory.graph.foreach(t => {
      drawElement(t._1,t._2)
    })

    //g.draw(TextShape("3efscdsfdsfdsfds",View.center))
   // g.draw(currentCategory.icon)
    g.draw(CircleShape(View.center, 130))


  }

  /**
   * Returns the direction in terms of MenuEvents, calculated from the angle
   * of a given point to the current center of the radial menu.
   */
  def direction(point : Vector2D) = {
    val angle  = (point - View.center).angle
    if      (angle > 345 || angle < 15)   EventE
    else if (angle > 15  && angle < 45)   EventENE
    else if (angle > 45  && angle < 75)   EventNNE
    else if (angle > 75  && angle < 105)  EventN
    else if (angle > 105 && angle < 135)  EventNNW
    else if (angle > 135 && angle < 165)  EventWNW
    else if (angle > 165 && angle < 195)  EventW
    else if (angle > 195 && angle < 225)  EventWSW
    else if (angle > 225 && angle < 255)  EventSSW
    else if (angle > 255 && angle < 285)  EventS
    else if (angle > 285 && angle < 315)  EventSSE
    else if (angle > 315 && angle < 345)  EventESE
    else                                  MenuEventNone
  }
}
