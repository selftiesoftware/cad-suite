/*
 * Copyright (c) 2008-2013, Selftie Software. Siigna is released under the
 * creative common license by-nc-sa. You are free
 *   to Share — to copy, distribute and transmit the work,
 *   to Remix — to adapt the work
 *
 * Under the following conditions:
 *   Attribution —   You must attribute the work to http://siigna.com in
 *                    the manner specified by the author or licensor (but
 *                    not in any way that suggests that they endorse you
 *                    or your use of the work).
 *   Noncommercial — You may not use this work for commercial purposes.
 *   Share Alike   — If you alter, transform, or build upon this work, you
 *                    may distribute the resulting work only under the
 *                    same or similar license to this one.
 *
 * Read more at http://siigna.com and https://github.com/siigna/main
 */
//
//package com.siigna.module.cad
//
//import com.siigna.app.model.DOM
//import com.siigna.module.Module
//import com.siigna.util.event.{Event,EventHandler}
//
//import com.siigna.util.action._
//import com.siigna.util.collection._
//
//class Show extends Module {
//
//val eventHandler = EventHandler(stateMap, stateMachine)
//
//  lazy val stateMap     = DirectedGraph('StartCategory     -> 'KeyEscape -> 'End)
//
//  def stateMachine = Map(
//    'StartCategory -> ((events : List[Event]) => {
//      goto('End)
//      Some(UpdateShapes(DOM.shapes, DOM.shapes.map(_.setAttribute("Visible" -> true))))
//    }),
//    'End -> ((events : List[Event]) => {
//    None
//    })
//  )
//
//
//}
