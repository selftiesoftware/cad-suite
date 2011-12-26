///* 2010 (C) Copyright by Siigna, all rights reserved. */
//
//package com.siigna.module.endogenous
//
//import com.siigna.module.Module
//
//import com.siigna.app.model.shape._
//import com.siigna.app.model.shape.collection._
//import com.siigna.app.view.Graphics
//import com.siigna.util.action._
//import com.siigna.util.collection.DirectedGraph
//import com.siigna.util.event._
//import com.siigna.util.geom._
//
//object Rectangle extends Module {
//
//  lazy val eventHandler = new EventHandler(stateMap, stateMachine)
//
//  var points = List[Vector2D]()
//
//  lazy val stateMap = DirectedGraph(
//    'Start       -> 'MouseClick  -> 'FirstPoint,
//    'FirstPoint  -> 'MouseMove   -> 'SecondPoint,
//    'SecondPoint -> 'MouseMove   -> 'SecondPoint,
//    'SecondPoint -> 'KeyEscape   -> 'End,
//    'SecondPoint -> 'MouseClick  -> 'End,
//    'Start       -> 'KeyEscape   -> 'End
//  )
//
//  lazy val stateMachine = Map(
//    'Start -> ((events : List[Event]) => {
//       None
//    }),
//    'FirstPoint -> ((events : List[Event]) => {
//      events match {
//        case MouseUp(point, MouseButtonLeft, _) :: tail => {
//          points = point :: Nil
//        }
//        case MouseUp(_, MouseButtonRight, _) :: tail => goto ('End)
//        case _ =>
//      }
//      None
//    }),
//    'SecondPoint -> ((events : List[Event]) => {
//      events match {
//        case MouseMove(point, _, _) :: tail => {
//          points = points.head :: point :: Nil
//        }
//        case _ =>
//      }
//      None
//    }),
//    'End -> ((events : List[Event]) => {
//      if (points.length == 2) Some(CreateShape(polylineFromPoints(points)))
//      else None
//    })
//  )
//
//  override def paint(g : Graphics, t : TransformationMatrix) {
//    if (points.length == 2) {
//      g draw polylineFromPoints(points).transform(t)
//    }
//  }
//
//  def polylineFromPoints(points : List[Vector2D]) = {
//    val p1 = points(0)
//    val p2 = points(1)
//    var pointShapes = List(PointShape(p1), PointShape(p2.x, p1.y),
//                           PointShape(p2), PointShape(p1.x, p2.y), PointShape(p1))
//    PolylineShape.fromPoints(pointShapes, true)
//  }
//
//}
