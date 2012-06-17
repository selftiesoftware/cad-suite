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

  def templateShape(shape : Shape) = {
    try {
      shape match {
        //export lineShapes
        case l : LineShape => println("GOT THE LINE")
      }
    }
  }

  def stateMap = DirectedGraph(
    'Start    ->   'KeyDown  ->    'End
  )

  def stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      println(Model.selection)
      //not possible. BUT WHY NOT!!!??!
      //println(Selection.shapes)

      /*
      try {
        templateShape match {
          case ArcShape => println("found arc")
          case LineShape => println("found line")
          case PolylineShape => println("found polyline")
          case _ => {
            Siigna display "Could not sample properties from that object"
            ForwardTo('Selection)
          }
        }
      }
      */
    }),
    'End -> ((events : List[Event]) => {

      //clear vars
      color = None
      weight = None
    })
  )
}