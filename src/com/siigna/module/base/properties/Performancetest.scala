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

import java.awt.Color._

import com.siigna._

/**
 * A performance test module which creates 1000*1000 lines.
 */
object Performancetest extends Module {

  var shapes = scala.collection.immutable.Vector[Shape]()
  val eventHandler = EventHandler(stateMap, stateMachine)

  val limit = 1000

  lazy val stateMap = DirectedGraph('Intermezzo -> 'KeyDown -> 'End)

  lazy val stateMachine = Map(

    'Start -> ((events : List[Event]) => {
      var i = 0
      val startSeconds = System.currentTimeMillis()

      do  {
          val pointA = Vector2D(i, 0)
          val pointB = Vector2D(i + 10, limit >> 1)

          val pointC = Vector2D(0, i)
          val pointD = Vector2D(limit >> 1, i + 10)

          //val vertShape = LineShape(pointA, pointB)
          val vertShape = LineShape(pointA, pointB)
          //val horizShape = LineShape(pointC, pointD)
          val horizShape = LineShape(pointC, pointD)

          i += 1

          shapes = shapes :+ vertShape :+ horizShape
      } while (i <= limit)

      val endSeconds = System.currentTimeMillis()
      Log("Time to draw "+ limit * limit +" lines:" + (endSeconds - startSeconds))
      Goto('Intermezzo)
    }),
    'Intermezzo -> ((events : List[Event]) => None),

    'End -> ((events : List[Event]) => {
    })
  )

  override def paint(g : Graphics, t : TransformationMatrix) {
    g setColor black
    shapes.map(_.transform(t)).foreach(g.draw)
  }
}