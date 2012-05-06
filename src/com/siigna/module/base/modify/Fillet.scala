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

package com.siigna.module.base.modify

import com.siigna.module.Module
import com.siigna._

/**
 * A module to connect two line segments with an arc of a given radius (of just join them if the radius is zero)
 */

object Fillet extends Module{

  val eventHandler = EventHandler(stateMap, stateMachine)

  def stateMap = DirectedGraph(
    'Start        -> 'KeyEscape  -> 'End
  )

  def stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      if(!Model.selection.isEmpty) {
        Model.deselect()
        Siigna display "select first line"
      } else Siigna display "select first line"
      Goto('SelectFirst)
    }),
    'SelectFirst -> ((events : List[Event]) => {
      ForwardTo('Selection)
      println(Model.selection.size)
      if(Model.selection.isDefined) {
        Siigna display "select second line"
        Goto('SelectSecond)
      }
    }),
    'SelectSecond -> ((events : List[Event]) => {
      println("in second")
      ForwardTo('Selection)
      println(Model.selection.size)
      if(Model.selection.size == 5) {
        Siigna display "type radius, default 0"
        Goto('Radius)
      } else ForwardTo('Selection)
    }),
    'Radius -> ((events : List[Event]) => {
       println("in radius")
    }),
    'End -> ((events : List[Event]) => {
      println("ending fillet")
      None
    })
  )
}