///* 2010 (C) Copyright by Siigna, all rights reserved. */
//
//package com.siigna.module.base
//
//import com.siigna.app.model.DOM
//import com.siigna.module.Module
//import com.siigna.util.event.{Event,EventHandler}
//
//import com.siigna.util.action._
//import com.siigna.util.collection._
//
//object Show extends Module {
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
