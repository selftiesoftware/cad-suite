///* 2010 (C) Copyright by Siigna, all rights reserved. */
//
//package com.siigna.module.endogenous
//
//import com.siigna.app.model.DOM
//import com.siigna.module.Module
//import com.siigna.util.event.{Event,EventHandler}
//
//import com.siigna.util.action._
//import com.siigna.util.collection._
//
//class Hide extends Module {
//
//val eventHandler = EventHandler(stateMap, stateMachine)
//
//  lazy val stateMap     = DirectedGraph('Start     -> 'KeyEscape -> 'End)
//
//  def stateMachine = Map(
//    'Start -> ((events : List[Event]) => {
//      val selection = DOM.getSelectedShapes
//
//      val hiddenSelection = selection.map(_.setAttribute("Visible" -> false))
//
//      goto('End)
//      Some(UpdateShapes(selection, hiddenSelection))
//    }),
//    'End -> ((events : List[Event]) => {
//    None
//    })
//  )
//
//
//}
