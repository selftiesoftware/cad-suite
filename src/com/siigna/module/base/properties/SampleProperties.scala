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

  var attributes : Attributes = Attributes()
  val eventHandler = EventHandler(stateMap, stateMachine)
  //var color : Option[String] = None
  var selected : Option[Selection] = None
  var templateShape : Option[Shape] = None
  //var weight : Option[String] = None

  def stateMap = DirectedGraph(
    'Start           ->   'KeyDown  ->    'End,
    'UpdateShapes    ->   'KeyDown  ->    'End
  )

  def stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      Siigna display ("select an object to sample from")

      if(Drawing.selection.isDefined && !Drawing.selection.get.isEmpty) {
        templateShape = Some(Drawing.selection.get.shapes.head._2)
        attributes = templateShape.get.attributes
        Drawing.deselect()
        Goto('UpdateShapes)
      }
      else ForwardTo('Selection, false)

    }),
    'UpdateShapes -> ((events : List[Event]) => {
      Siigna display ("select objects to update")
      if(Drawing.selection.isDefined && !Drawing.selection.get.isEmpty) Goto('End)
      else ForwardTo('Selection, false)
    }),
    'End -> ((events : List[Event]) => {
      if(Drawing.selection.isDefined && !Drawing.selection.get.isEmpty) {
        Drawing.selection.get.setAttributes(attributes)
        Drawing.deselect()
      }
      //clear vars
      attributes = Attributes()
      selected = None
      templateShape = None
    })
  )
}