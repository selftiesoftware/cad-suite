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

/*package com.siigna.module.base.helpers

import com.siigna._
import com.siigna.module.Module
import com.siigna.module.base.Default._
/**
 * A module to draw a grid - with a gridsize set by the user - on the paper.
 */

object Grid extends Module{

  //default grid scale
  var scale : Double = 10

  var text = ""

  def eventHandler = EventHandler(stateMap, stateMachine)

  def stateMap = DirectedGraph (
    'StartCategory -> 'KeyEscape -> 'End
  )

  lazy val stateMachine = Map(
    'StartCategory -> ((events : List[Event]) => {
      if(gridIsOn == false) {
        Siigna display "Type grid scale: "+ text +" Current: "+ scale +" millimeters"
        events match {
          case KeyDown(Key.Backspace, _) :: tail => {
              if (text.length != 0) text = text.substring(0, text.length - 1)
              else Goto('End)
          }
          case KeyDown(Key.Enter, _) :: tail => {
            if(text.length() == 0) Goto('End)
            else {
              scale = text.toInt
              text = ""
              gridIsOn = true
              Goto('End)
            }
          }
          case KeyDown(Key.Esc, _) :: tail => {
            text = ""
            Goto('End)
          }
          case KeyDown(key, _) :: tail => {
            text += key.toChar.toString.toLowerCase
            Siigna display "Type grid scale: "+ text +" Current: "+ scale +" millimeters"
          }
          case MouseMove(_, _, _) :: tail => {
            gridIsOn = true
            Goto('End)
          }
          case MouseUp(_, MouseButtonRight, _) :: tail => Goto('End)
          case MouseDown(_, MouseButtonRight, _) :: tail => Goto('End)
          case MouseDown(_, MouseButtonLeft, _) :: tail =>
          case _ =>
        }
      }else if(gridIsOn == true) {
        gridIsOn = false
        Goto('End)
      }
    }),
    'End -> ((events : List[Event]) => {
      if (gridIsOn == false) Siigna display "Grid is off"

    })
  )

  //this paint method is called in 'Default, which means it is drawn always (if toggled of course)
  override def paint(g : Graphics, t : TransformationMatrix) {

    // Get the boundary
    val boundary = Drawing.boundary
    val topLeft = boundary.topLeft
    val topRight = boundary.topRight
    val bottomLeft = boundary.bottomLeft

    //draw major horizontal lines
    for(i <- 0 to ((boundary.height*0.96)/scale).toInt) {
      var offsetY = Vector2D(0,i*scale)
      g draw LineShape(topRight - offsetY, topLeft - offsetY).setAttribute("Color" -> "#EEEEEE".color).transform(t)
    }
    //draw major vertical lines
    for(i <- 0 to (boundary.width/scale).toInt) {
      var offsetX = Vector2D(i*scale,0)
      g draw LineShape(topLeft + offsetX, Vector2D(bottomLeft.x, bottomLeft.y + boundary.height*0.04) + offsetX).setAttribute("Color" -> "#EEEEEE".color).transform(t)
    }
  }
}
*/