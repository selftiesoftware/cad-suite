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

/* 2011 (C) Copyright by Siigna, all rights reserved. */

import com.siigna._

object Arc extends Module {

  val eventHandler = EventHandler(stateMap, stateMachine)

  var points : List[Vector2D] = List()

  var previousPoint : Option[Vector2D] = None

  def stateMap = DirectedGraph(
    'Start         -> 'MouseMove -> 'Start,
    'Start         -> 'KeyEscape -> 'End,
    'Start         -> 'Action    -> 'FirstPoint,
    'FirstPoint    -> 'Action    -> 'SecondPoint,
    'FirstPoint    -> 'KeyEscape -> 'End,
    'SecondPoint   -> 'KeyEscape -> 'End,
    'SecondPoint   -> 'Action    -> 'End
  )

  def stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      if (previousPoint.isEmpty)
        ForwardTo('Point)
      else {
        points = List(previousPoint.get)
        Goto('FirstPoint)
      }
      None
    }),
    'FirstPoint -> ((events : List[Event]) => {
      events match {
        case Message(p : Vector2D) :: tail => {
          points = List(p)
        }
        case _ =>
      }
      val guide : Vector2D => ImmutableShape = point => {
        val normalVector2D = (points(0) - point).normal
        val middle = normalVector2D + ((points(0) + point) / 2)
        ArcShape(points(0), middle, point)
      }
      //ForwardTo('Point)
      //Message(PointGuide(guide))
    }),
    'SecondPoint -> ((events : List[Event]) => {
      events match {
        case Message(p : Vector2D) :: tail => {
          if (points.size > 1)
            Goto('End)
          else
            points = points.::(p)
        }
        case _ =>
      }
      val guide : Vector2D => ImmutableShape = point => {
        val normalVector2D = (points(0) - points(1)).normal
        val middle = normalVector2D + ((points(0) + points(1)) / 2)
        ArcShape(points(0), point, points(1))
      }
      //ForwardTo('Point)
      //Message(PointGuide(guide))
      None
    }),
    'End -> ((events : List[Event]) => {
      events match {
        case Message(p : Vector2D) :: tail => {
          Create(ArcShape(points(1), p, points(0)))
        }
        case _ => None
      }
    })
  )

}