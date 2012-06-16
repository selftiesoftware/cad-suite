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

package com.siigna.module.base.properties

import com.siigna._

/**
 * A module to transfer properties from one shape to a selection of shapes.
 */

object SampleProperties extends Module{

  val eventHandler = EventHandler(stateMap, stateMachine)
  var color : Option[String] = None
  var weight : Option[String] = None

  def stateMap = DirectedGraph(
    'Start    ->   'KeyDown  ->    'End
  )

  def stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      println(Model.selection.toList)
      println(Model.selection)
      if(Model.selection.isDefined && (!color.isDefined && !weight.isDefined) == true) {
         Siigna display "select an object to sample"
         println("goint to selection")
         ForwardTo('Selection, false)
       } else if(Model.selection.size > 1){
         Siigna display  "please select just one object"

       } else {

         Goto('End)
       }
    }),
    'End -> ((events : List[Event]) => {

      //clear vars
      color = None
      weight = None
    })
  )
}