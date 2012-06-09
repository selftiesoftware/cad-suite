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

/* 2010 (C) Copyright by Siigna, all rights reserved. */

import com.siigna.module.Module

import com.siigna._

/**
 * Created by IntelliJ IDEA.
 * User: oep
 * Date: 09-06-12
 * Time: 11:48
 * To change this template use File | Settings | File Templates.
 */

object Explode extends Module{

  lazy val eventHandler = new EventHandler(stateMap, stateMachine)

  lazy val stateMap = DirectedGraph(
    'Start     -> 'KeyEscape  -> 'End
  )

  val shapes : List[Shape] = List[]

  lazy val stateMachine = Map(

    'Start -> ((events : List[Event]) => {
      if(Model.selection.isDefined) Goto('Explode)
    }),

    'Explode -> ((events : List[Event]) => {
      def explode (shape : Shape) = {
        try {
          shape match {
            case a : ArcShape => {
              println("found LineShape - this shape cannot be exploded.: "+a)

            }
            case c : CircleShape => {
              println("found CircleShape - this shape cannot be exploded yet.: "+c)

            }
            case p : PolylineShape => {
              println("explode polylines here: "+p)

            }
            case l : LineShape => {
              println("found LineShape - this shape cannot be exploded.: "+l)

            }
            case _ => Siigna display "Some shapes could not be exploded"
          }
        }
      }
      Goto('End)
    }),

    'End -> ((events : List[Event]) => {
      Siigna display "Explode XXX objects"
   })
  )
}