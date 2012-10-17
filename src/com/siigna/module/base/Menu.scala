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
import com.siigna.module.base.radialmenu._

import com.siigna._

import category.{Create => MenuCreate, Start, MenuCategory}

/**
 * The menu module. This module shows the menu as radial items and categories in 13 places (directions):
 * N, NNE, ENE, E, ESE, SSE, S, SSW, WSW, W, WNW and NNW. There is also a center (C) place.
 */
object Menu extends Module {

  // The center of the radial menu
  var center : Option[Vector2D]         = None

  // The current active category
  var currentCategory : MenuCategory  = Start

  // The distance to draw the icons; Used in the initiation of the menu module to animate the icons.
  private var distanceScale : Double = 1

  //defining the parameters needed to start the menu
  def initializeMenu() {

    // Make sure the rest of the program doesn't move
    Siigna.navigation = false

    // Disable tracking and snapping
    eventParser.disable
  }

  //a var to pass on the last key down back to Default, to check if it is needed to activate a shortcut
  var lastKey : Option[KeyDown] = None

  var moduleCallFromMenu : Boolean = false

  /**
   * The radius of the wheel. Can be adjusted to adjust the size of the wheel.
   */
  var radius = 180

  // The transformation to use throughout the paint
  private var transformation = TransformationMatrix(Vector2D(0, 0), 1)

  def stateMap = Map(
    'Start -> {
      case _ => 'Interaction
    },
    'Interaction -> {
      case MouseDown(_, MouseButtonRight, _) :: tail => {
        println("Right")
        'End
      }
    },
    State('End, () => (
      println("Ending")
    ))
  )

  /**
   * Paints the menu. If the menu is being initalized we multiply the transformationMatrix with a <code>distanceScale</code> to show an animation
   * of the icons, origining from the center.
   */
  override def paint(g : Graphics, transformation : TransformationMatrix) {
    println("Menu")
  }

}
