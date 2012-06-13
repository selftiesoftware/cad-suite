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
    'Start     -> 'KeyDown  -> 'End
  )

  var explodedPolylines : Int = 0

  lazy val stateMachine = Map(

    'Start -> ((events : List[Event]) => {
      events match {
        // GENERAL NOTE: after the Menu forwards to a module with the (presumably) last events,
        // other events may be registered while the ForwardTo mechanism is running.
        // These events can trigger Goto events in the module unless ruled out by case matches.
        case MouseUp(_, _, _) :: tail => {
          if(Model.selection.isDefined) {
            Goto('Explode)
          }
        }
        case _ =>
      }
    }),

    'Explode -> ((events : List[Event]) => {
      def explode (shape : Shape) : Seq[Shape] = {
        try {
          shape match {
            case a : ArcShape => {
              println("found LineShape - this shape cannot be exploded"+a)
              Seq()
            }
            case c : CircleShape => {
              println("found CircleShape - this shape cannot be exploded yet"+c)
              Seq()
            }
            case p : PolylineShape => {
              //println("explode polylines here: "+p)
              explodedPolylines += 1

              p.shapes

            }
            case l : LineShape => {
              println("found LineShape - this shape cannot be exploded")
              Seq()
            }
            case _ => Siigna display "Some shapes could not be exploded"
            Seq()
          }
        }
      }
      //filter everything from the Seq that is empty.
      //run the explode def on the rest, and save the restults to the explodeableShapes val.
      val explodeableShapes = Model.selection.get.shapes.map(t => t._1 -> explode(t._2)).filter(t => !t._2.isEmpty)

      if(!explodeableShapes.isEmpty) {
        //use the IDs from the Seq to delete the original shapes
        Delete(explodeableShapes.keys)
        //use the shapeParts to create the exploded shapes
        Create(explodeableShapes.values.flatten)
      }

      Siigna display "Exploded "+explodedPolylines+" polylines to lines"
      Goto('End)
    }),
    'End -> ((events : List[Event]) => {
   })
  )
}