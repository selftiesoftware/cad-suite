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

import com.siigna._

object Selection extends Module {

  val eventHandler = EventHandler(stateMap, stateMachine)

  private var box : Option[Rectangle2D] = None

  private var selectFullyEnclosed : Boolean = false

  // The starting point of the rectangle
  private var startPoint : Option[Vector2D] = None

  /**
   * Examines whether the selection is currently enclosed or not.
   */
  def isEnclosed : Boolean = startPoint.isDefined && startPoint.get.x <= Siigna.mousePosition.x

  def stateMap     = DirectedGraph(
    'Start -> 'MouseDrag   -> 'Box,
    'Start -> 'MouseMove   -> 'End,
    'Start -> 'MouseUp     -> 'End,
    'Box   -> 'MouseMove   -> 'End,
    'Box   -> 'MouseUp     -> 'End
  )

  Preload('Move, "com.siigna.module.base.modify")
  Preload('Copy, "com.siigna.module.base.create")
  def stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      events match {
        case MouseDown(p, _, _) :: tail => startPoint = Some(p)
        case MouseMove(p, _, _) :: tail => startPoint = Some(p)
        case MouseDrag(p, _, _) :: tail => startPoint = Some(p)
        case _ =>
      }
      if (Default.nearestShape.isDefined) {
        val shape = Default.nearestShape.get
        val part = shape._2.getPart(Siigna.mousePosition)
        Model.select(shape._1, part)
        Goto('End)
        ForwardTo('Move)
      }
    }),
    'Box -> ((events : List[Event]) => {
      if (startPoint.isEmpty) {
        Goto('End)
      } else {
        events match {
          case MouseDrag(p, _, _) :: tail => {
            var startX = startPoint.get.x
            if(startPoint.get.x < p.x) {
              selectFullyEnclosed = true
              box = Some(Rectangle2D(startPoint.get, p))
            }
            else {
              selectFullyEnclosed = false
              box = Some(Rectangle2D(startPoint.get, p))
            }
          }
          case _ => Goto('End)
        }
      }
    }),
    'End -> ((events : List[Event]) => {
      //if the selection is drawn from left to right, select fully enclosed shapes only.:
      if (box.isDefined && selectFullyEnclosed == true) {
        Select(box.get, true)
        box = None
      }
      //if the selection is drawn from right to left, select partially enclosed shapes as well.:
      else if (box.isDefined && selectFullyEnclosed == false) {
        Select(box.get, false)
        box = None
      } else None
    })
  )

  override def paint(g : Graphics, t : TransformationMatrix) {
    val enclosed = "Color" -> "#9999FF".color
    val focused  = "Color" -> "#FF9999".color

    if (box.isDefined) {
      g draw PolylineShape.fromRectangle(box.get).addAttribute("Color" -> (if (isEnclosed) enclosed else focused)).transform(t)
    }

  }
}