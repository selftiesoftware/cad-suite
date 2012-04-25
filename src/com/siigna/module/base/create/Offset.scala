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

object Offset extends Module {

  val eventHandler = EventHandler(stateMap, stateMachine)

  def stateMap = DirectedGraph(
    'Start         -> 'KeyEscape ->         'End
  )

  //Select shapes
  def stateMachine = Map(
  'Start -> ((events : List[Event]) => {
    events match {

      case MouseUp(_, MouseButtonRight, _) :: tail => Goto('End)
      case _ => {
        /*if(Model.isSelected){
          templateShape = Some(Model.selected)
          Goto('SetDistance)
        }*/
      }
    }
  None
  }),

  //Forward to Point to get offset point
  'SetDistance -> ((events : List[Event]) => {
    val offsetGuide : Vector2D => LineShape = (v : Vector2D) => {
       LineShape(Vector2D(0,0),Vector2D(100,100))
     }

    events match {
      case MouseUp(_, MouseButtonRight, _) :: tail => Goto('End)
      case Message(p : Vector2D) :: tail => {
        println("got point: "+p)
        Controller ! Message(offsetGuide)
      }
      case _ => ForwardTo('Point, false)
    }
  None
  }),
  //do the offset calculation here
  'Offset -> ((events : List[Event]) => {
    events match {
      case MouseUp(_, MouseButtonRight, _) :: tail => Goto('End)

      case _ => ForwardTo('Point)
    }
  None
  }),

    //do the offset calculation here
    'End -> ((events : List[Event]) => {

  }))
}