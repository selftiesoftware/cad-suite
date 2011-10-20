/* 2011 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

import com.siigna._

class Arc extends Module {

  val eventHandler = EventHandler(stateMap, stateMachine)

  var points : List[Vector] = List()

  var previousPoint : Option[Vector] = None

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
        case Message(p : PointShape) :: tail => {
          points = List(p.point)
        }
        case _ =>
      }
      val guide : Vector => ImmutableShape = point => {
        val normalVector = (points(0) - point).normal
        val middle = normalVector + ((points(0) + point) / 2)
        ArcShape(points(0), middle, point)
      }
      ForwardTo('Point)
      Message(PointGuide(guide))
    }),
    'SecondPoint -> ((events : List[Event]) => {
      events match {
        case Message(p : PointShape) :: tail => {
          if (points.size > 1)
            Goto('End)
          else
            points = points.::(p.point)
        }
        case _ =>
      }
      val guide : Vector => ImmutableShape = point => {
        val normalVector = (points(0) - points(1)).normal
        val middle = normalVector + ((points(0) + points(1)) / 2)
        ArcShape(points(0), point, points(1))
      }
      ForwardTo('Point)
      Message(PointGuide(guide))
      None
    }),
    'End -> ((events : List[Event]) => {
      events match {
        case Message(p : PointShape) :: tail => {
          Create(ArcShape(points(1), p.point, points(0)))
        }
        case _ => None
      }
    })
  )

}