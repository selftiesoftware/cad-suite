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

package com.siigna.module.cad.create
/*
import com.siigna._
import app.controller.Controller

/**
 * trying out som stuff..
 */

object ParamTest extends Module{

val eventHandler = new EventHandler(stateMap, stateMachine)

  var points = List[Vector2D]()

  var rectangle : Option[PolylineShape] = None

  val stateMap = DirectedGraph(
    'Start       -> 'Message   -> 'SetPoint
  )

  lazy val stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      events match {
        case MouseDown(_, MouseButtonRight, _) :: tail => Goto('End)
        case Message(p : Vector2D) :: tail => Goto('SetPoint)
        case _ => ForwardTo('Point, false)

      }
    }),
    'SetPoint -> ((events : List[Event]) => {
      //A function that passes a rectangleShape to the point module to be drawn dynamically
      //from the first point to the mouse position
      val getRectGuide : Vector2D => PolylineShape = (v : Vector2D) => {
        PolylineShape(SimpleRectangle2D(points.head, v))
      }

      events match {
        case Message(point : Vector2D) :: tail => {
          if(points.length == 1) {
            points = points :+ point
            Goto('ParamTest)
          } else if (points.length == 0) {
           points = points :+ point
           //Controller ! Message(PointGuide(getRectGuide))
           ForwardTo('Point)
          }
        }
        case _ =>
      }
    }),
    'ParamTest -> ((events : List[Event]) => {
      if (points.length == 2) {
        Create(PolylineShape(SimpleSimpleRectangle2D(points(0), points(1))))
        for(i <- 0 to 100) {
          var dynamicCorner = points(1) + Vector2D(i,i)
          rectangle = Some(PolylineShape(SimpleRectangle2D(points(0), dynamicCorner)))
          Thread.sleep(10)
        }
      Goto('End)
      }

      // Clear variables
      points = List[Vector2D]()
      com.siigna.module.base.ModuleInit.previousModule = Some('ParamTest)
    }),
    'End -> ((events : List[Event]) => {

    })
  )

  override def paint(g : Graphics, t : TransformationMatrix) {
    if (rectangle.isDefined)
    g draw rectangle.get.transform(t)

  }

}
*/