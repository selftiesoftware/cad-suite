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
      selected = Model.selection
      Siigna display ("select an object to sample from")

      if(!Model.selection.get.shapes.isEmpty) templateShape = Some(Model.selection.get.shapes.head._2)
      println("shape: "+Model.selection.get.shapes.head._2)

      //store the attributes of the selected shape. TODO: not working, .attributes always yields Attributes()
      attributes = templateShape.get.attributes
      println(templateShape.get.attributes)
      Model.deselect()
      Goto('UpdateShapes)
    }),
    'UpdateShapes -> ((events : List[Event]) => {
      Siigna display ("select objects to update")
      ForwardTo('Selection, false)
      if(!Model.selection.get.shapes.isEmpty) {
        println("got objects to update")
        Goto('End)
      }
    }),
    'End -> ((events : List[Event]) => {
      println("in end")
      if(!Model.selection.get.shapes.isEmpty) {
        println("model not empty")
        println("sampled attributes: "+attributes)
        Model.selection.get.setAttributes(attributes)
        Model.deselect()
      }
      //clear vars
      attributes = Attributes()
      selected = None
      templateShape = None
    })
  )
}