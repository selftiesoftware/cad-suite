/*
 * Copyright (c) 2008-2013. Siigna is released under the creative common license by-nc-sa. You are free
 * to Share — to copy, distribute and transmit the work,
 * to Remix — to adapt the work
 *
 * Under the following conditions:
 * Attribution —  You must attribute the work to http://siigna.com in the manner specified by the author or licensor (but not in any way that suggests that they endorse you or your use of the work).
 * Noncommercial — You may not use this work for commercial purposes.
 * Share Alike — If you alter, transform, or build upon this work, you may distribute the resulting work only under the same or similar license to this one.
 */

package com.siigna.module.cad.modify

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import com.siigna.util.geom._
import com.siigna.app.model.shape.{PolylineLineShape, PolylineShape}
import com.siigna.util.collection.Attributes
import com.siigna.app.model.shape.PolylineShape.{PolylineShapeClosed, PolylineShapeOpen}

/**
 * Tests the [[com.siigna.module.cad.modify.Trim]] class.
 */

class TrimSpec extends FunSpec with ShouldMatchers {

  val attr = Attributes()
  val gs1 = PolylineShape(List(Vector2D(-45,20),Vector2D(-45,-30)))
  val gs2 = PolylineShape(List(Vector2D(-10,20),Vector2D(-15,-30)))
  val gs3 = PolylineShape(List(Vector2D(10,20),Vector2D(10,-30)))
  val gs4 = PolylineShape(List(Vector2D(20,20),Vector2D(20,-30)))
  val gs5 = PolylineShape(List(Vector2D(30,20),Vector2D(40,-30)))
  val gs6 = PolylineShape(List(Vector2D(60,20),Vector2D(60,-30)))
  val gs7 = PolylineShape(List(Vector2D(70,20),Vector2D(70,-30)))
  val trimLine = PolylineShape(List(Vector2D(-200,-30),Vector2D(-100,0),Vector2D(10,10), Vector2D(100,0),Vector2D(200,30)))
  val twoGuides = List(gs1, gs2) //both guides on a segment
  val twoGuidesOnSegment = List(gs2, gs3) //one of the guides sits on a segment's endVertex
  val sevenGuides = List(gs1, gs2, gs3, gs4, gs5, gs6, gs7)

  val p1 = Vector2D(55,5)
  val p2 = Vector2D(0,9.09175)
  val p3 = Vector2D(-50,4.54321)
  val p4 = Vector2D(-200,-30) //point at far left
  val p5 = Vector2D(0,-50) // a point not on the trimLine
  val p6 = Vector2D(10,10) // a point on an endVertex and a guideShape
  val p7 = Vector2D(200,30) // point at far right

  //TODO: test trimming of a rotated rect inside a larger rect, creating four intersections:
  /*
  describe("polylines... ") {

    it("intersections can be found with another PL") {
      val trimLine2 = PolylineShape(List(Vector2D(0,-10),Vector2D(0,0),Vector2D(10,0), Vector2D(10,-10)))
      val trimLine3 = PolylineShape(List(Vector2D(10,-10),Vector2D(10,0),Vector2D(0,0), Vector2D(0,-10)))
      val oneGuide = List(PolylineShape(List(Vector2D(0,30),Vector2D(0,-30))))

      TrimmingMethods.getIntersectSegmentNumbers(twoGuides,trimLine) should equal(Map(0 -> List(), 1 -> List(Vector2D(-45.0,5.0), Vector2D(-11.192660550458712,8.073394495412845)), 2 -> List(), 3 -> List()))
      TrimmingMethods.getIntersectSegmentNumbers(oneGuide,trimLine2) should equal(Map(0 -> List(), 1 -> List(Vector2D(0.0,0.0)), 2 -> List()))
      //trimLine reversed
      TrimmingMethods.getIntersectSegmentNumbers(oneGuide,trimLine3) should equal(Map(0 -> List(), 1 -> List(Vector2D(0.0,0.0)), 2 -> List()))
    }

    //findIntersection tests:

    it("segment ID and Segment on which a point is set can be returned") {
      TrimmingMethods.findIntSegNrAtPoint(trimLine,p1) should equal (Some((2,Segment2D(Vector2D(10.0,10.0),Vector2D(100.0,0.0)))))
      TrimmingMethods.findIntSegNrAtPoint(trimLine,p2) should equal (Some((1,Segment2D(Vector2D(-100.0,0.0),Vector2D(10.0,10.0)))))
      TrimmingMethods.findIntSegNrAtPoint(trimLine,p3) should equal (Some((1,Segment2D(Vector2D(-100.0,0.0),Vector2D(10.0,10.0)))))
      TrimmingMethods.findIntSegNrAtPoint(trimLine,p4) should equal (Some((0,Segment2D(Vector2D(-200.0,-30.0),Vector2D(-100.0,0.0)))))
      TrimmingMethods.findIntSegNrAtPoint(trimLine,p5) should equal (None)
    }

    it("first relevant intersection (to be used for trimming) in a given direction from the trim point can be found") {
      val twoInts = TrimmingMethods.getIntersectSegmentNumbers(twoGuides,trimLine)
      //one of the guide lines is on top of a vertex:
      val intsTlOnNode = TrimmingMethods.getIntersectSegmentNumbers(twoGuidesOnSegment,trimLine)
      val sevenInts = TrimmingMethods.getIntersectSegmentNumbers(sevenGuides,trimLine)

      TrimmingMethods.findIntersection(trimLine,twoInts,2,true,Some(p1)) should equal(None) //positive direction
      TrimmingMethods.findIntersection(trimLine,twoInts,2,false,Some(p1)) should equal(Some((1,Vector2D(-11.192660550458712,8.073394495412845)))) //negative direction
      TrimmingMethods.findIntersection(trimLine,intsTlOnNode,2,false,Some(p1)) should equal(Some((2,Vector2D(10.0,10.0)))) //negative direction
      TrimmingMethods.findIntersection(trimLine,sevenInts,2,true,Some(p1)) should equal (Some((2,Vector2D(60.0,4.444444444444445)))) //positive direction
      TrimmingMethods.findIntersection(trimLine,sevenInts,2,false,Some(p1)) should equal (Some((2,Vector2D(32.5,7.5)))) //negative direction

      //ints not on same segment as the one selected with the mouse
      TrimmingMethods.findIntersection(trimLine,sevenInts,3,false,Some(p7)) should equal (Some((2,Vector2D(70.0,3.333333333333334)))) //negative direction, point far left
      TrimmingMethods.findIntersection(trimLine,sevenInts,0,true,Some(p4)) should equal (Some((1,Vector2D(-45.0,5.0)))) //positive direction, point far left
    }

    it("resultant trimLines at int2 situations can be generated... ") {
      val shapes = Map(-2 -> PolylineShapeOpen(Vector2D(30.0,150.0),List(PolylineLineShape(Vector2D(-20.0,30.0)), PolylineLineShape(Vector2D(-10.0,20.0)), PolylineLineShape(Vector2D(-15.0,-30.0))), Attributes()), -3 -> PolylineShapeOpen(Vector2D(10.0,20.0),List(PolylineLineShape(Vector2D(10.0,-30.0))), Attributes()))

      TrimmingMethods.trimPolyline(shapes,trimLine,p1)._1 should equal(None)
      TrimmingMethods.trimPolyline(shapes,trimLine,p1)._2 should equal(Some(List(Vector2D(-200.0,-30.0), Vector2D(-100.0,0.0), Vector2D(10.0,10.0), Vector2D(10.0,10.0))))
    }

    it("can be trimmed by a pl which intersects the same segment of the tl multiple times... ") {
      val trimLine = PolylineShape(List(Vector2D(-20,0),Vector2D(-20,20),Vector2D(20,20), Vector2D(20,0)))
      val gs = Map(-4 -> PolylineShapeOpen(Vector2D(-10,0),List(PolylineLineShape(Vector2D(-10,30)),PolylineLineShape(Vector2D(10,30)),PolylineLineShape(Vector2D(10,0))),Attributes()))
      val tp = Vector2D(0,20)

      TrimmingMethods.trimPolyline(gs,trimLine,tp)._1 should equal(Some(List(Vector2D(10.0,20.0), Vector2D(20.0,20.0), Vector2D(20.0,0.0))))
      TrimmingMethods.trimPolyline(gs,trimLine,tp)._2 should equal(Some(List(Vector2D(-20.0,00.0), Vector2D(-20.0,20.0), Vector2D(-10.0,20.0))))
    }
  }
  */
  //TODO: implement trimming of closed polylines
  describe("closed polylines... ") {
    it("should not be trimmed when there is only one intersection") {
      val trimLine = PolylineShape(Rectangle2D(Vector2D(-20,-20),Vector2D(20,20)))
      val gs = Map(-4 -> PolylineShapeOpen(Vector2D(0,0),List(PolylineLineShape(Vector2D(0,30))),Attributes()))
      val tp = Vector2D(20,10)

      TrimmingMethods.trimPolylineClosed(gs,trimLine,tp) should equal(None)
    }
    it("should be trimmed with two intersections") {
      val trimLine = PolylineShape(Rectangle2D(Vector2D(-20,-20),Vector2D(20,20)))
      val gs = Map(-4 -> PolylineShapeOpen(Vector2D(0,-30),List(PolylineLineShape(Vector2D(0,30))),Attributes()))
      val tp = Vector2D(20,0)
      val tp2 = Vector2D(-20,0)

      TrimmingMethods.trimPolylineClosed(gs,trimLine,tp) should equal(Some(List(Vector2D(0.0,-20.0), Vector2D(-20.0,-20.0), Vector2D(-20.0,20.0),Vector2D(0.0,20))))

      TrimmingMethods.trimPolylineClosed(gs,trimLine,tp2) should equal(Some(List(Vector2D(0.0,20.0), Vector2D(20.0,-20.0), Vector2D(20.0,20.0),Vector2D(0.0,20))))
    }
  }
}