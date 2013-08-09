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
import com.siigna.app.model.shape.{LineShape, PolylineLineShape, PolylineShape}
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
      val trimLine4 = PolylineShape(List(Vector2D(-20,-10),Vector2D(-20,20),Vector2D(20,20), Vector2D(20,-10)))

      val oneGuide = List(PolylineShape(List(Vector2D(0,30),Vector2D(0,-30))))
      val oneGuide2 = List(PolylineShape(List(Vector2D(-30,0),Vector2D(30,0))))

      TrimmingMethods.getIntersectSegmentNumbers(twoGuides,trimLine) should equal(Map(0 -> List(), 1 -> List(Vector2D(-45.0,5.0), Vector2D(-11.192660550458712,8.073394495412845)), 2 -> List(), 3 -> List()))
      TrimmingMethods.getIntersectSegmentNumbers(oneGuide,trimLine2) should equal(Map(0 -> List(), 1 -> List(Vector2D(0.0,0.0)), 2 -> List()))
      //trimLine reversed
      TrimmingMethods.getIntersectSegmentNumbers(oneGuide,trimLine3) should equal(Map(0 -> List(), 1 -> List(Vector2D(0.0,0.0)), 2 -> List()))

      //three segment trimline with two ints (on seg 0 and 2)
      TrimmingMethods.getIntersectSegmentNumbers(oneGuide2,trimLine4) should equal(Map(0 -> List(Vector2D(-20.0,0.0)), 1 -> List(), 2 -> List(Vector2D(20.0,0.0))))
    }

    //findIntersectionOpen tests:

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

      TrimmingMethods.findIntersectionOpen(trimLine,twoInts,2,true,Some(p1)) should equal(None) //positive direction
      TrimmingMethods.findIntersectionOpen(trimLine,twoInts,2,false,Some(p1)) should equal(Some((1,Vector2D(-11.192660550458712,8.073394495412845)))) //negative direction
      TrimmingMethods.findIntersectionOpen(trimLine,intsTlOnNode,2,false,Some(p1)) should equal(Some((2,Vector2D(10.0,10.0)))) //negative direction
      TrimmingMethods.findIntersectionOpen(trimLine,sevenInts,2,true,Some(p1)) should equal (Some((2,Vector2D(60.0,4.444444444444445)))) //positive direction
      TrimmingMethods.findIntersectionOpen(trimLine,sevenInts,2,false,Some(p1)) should equal (Some((2,Vector2D(32.5,7.5)))) //negative direction

      //ints not on same segment as the one selected with the mouse
      TrimmingMethods.findIntersectionOpen(trimLine,sevenInts,3,false,Some(p7)) should equal (Some((2,Vector2D(70.0,3.333333333333334)))) //negative direction, point far left
      TrimmingMethods.findIntersectionOpen(trimLine,sevenInts,0,true,Some(p4)) should equal (Some((1,Vector2D(-45.0,5.0)))) //positive direction, point far left
    }


    it("both relevant intersections (to be used for trimming) can be found when the trimpoint is on the same segment as either intersection") {
      val trimLine = PolylineShape(List(Vector2D(-20,-10),Vector2D(-20,20),Vector2D(20,20), Vector2D(20,-10)))
      val gs = List(LineShape(Vector2D(-30,0),Vector2D(30,0)))
      val tp = Vector2D(-20,10)

      val ints = TrimmingMethods.getIntersectSegmentNumbers(gs,trimLine)

      TrimmingMethods.findIntersectionOpen(trimLine,ints,0,true,Some(tp)) should equal(Some((2,Vector2D(20.0,0.0)))) //positive direction
      TrimmingMethods.findIntersectionOpen(trimLine,ints,0,false,Some(tp)) should equal(Some((0,Vector2D(-20.0,0.0)))) //negative direction

    }


    it("resultant trimLines at int2 situations can be generated... ") {
      val shapes = Map(-2 -> PolylineShapeOpen(Vector2D(30.0,150.0),List(PolylineLineShape(Vector2D(-20.0,30.0)), PolylineLineShape(Vector2D(-10.0,20.0)), PolylineLineShape(Vector2D(-15.0,-30.0))), Attributes()), -3 -> PolylineShapeOpen(Vector2D(10.0,20.0),List(PolylineLineShape(Vector2D(10.0,-30.0))), Attributes()))
      val trimLine2 = PolylineShape(List(Vector2D(-20,-10),Vector2D(-20,20),Vector2D(20,20), Vector2D(20,-10)))
      val gs = Map(-4 -> LineShape(Vector2D(-30,0),Vector2D(30,0)))
      val tp1 = Vector2D(20,10)
      val tp2 = Vector2D(-20,10)

      TrimmingMethods.trimPolylineOpen(shapes,trimLine,p1)._1 should equal(None)
      TrimmingMethods.trimPolylineOpen(shapes,trimLine,p1)._2 should equal(Some(List(Vector2D(-200.0,-30.0), Vector2D(-100.0,0.0), Vector2D(10.0,10.0), Vector2D(10.0,10.0))))

      //trimming with two intersections, regardless of which trimline segment is clicked

      /* 1
      *----*
  TP1*|    |*TP2
    ----*----*---- gs
      |0   |2

      */

      //TP on int1 segment
      TrimmingMethods.trimPolylineOpen(gs,trimLine2,tp1)._1 should equal(Some(List(Vector2D(20.0,0.0), Vector2D(20.0,-10.0))))
      TrimmingMethods.trimPolylineOpen(gs,trimLine2,tp1)._2 should equal(Some(List(Vector2D(-20.0,-10.0), Vector2D(-20.0,0.0))))



      //CCW
      TrimmingMethods.trimPolylineOpen(gs,trimLine2,tp2)._1 should equal(Some(List(Vector2D(20.0,0.0), Vector2D(20.0,-10.0))))
      TrimmingMethods.trimPolylineOpen(gs,trimLine2,tp2)._2 should equal(Some(List(Vector2D(-20.0,-10.0), Vector2D(-20.0,0.0))))

    }

    it("can be trimmed by a pl which intersects the same segment of the tl multiple times... ") {
      val trimLine = PolylineShape(List(Vector2D(-20,0),Vector2D(-20,20),Vector2D(20,20), Vector2D(20,0)))
      val gs = Map(-4 -> PolylineShapeOpen(Vector2D(-10,0),List(PolylineLineShape(Vector2D(-10,30)),PolylineLineShape(Vector2D(10,30)),PolylineLineShape(Vector2D(10,0))),Attributes()))
      val tp = Vector2D(0,20)

      TrimmingMethods.trimPolylineOpen(gs,trimLine,tp)._1 should equal(Some(List(Vector2D(10.0,20.0), Vector2D(20.0,20.0), Vector2D(20.0,0.0))))
      TrimmingMethods.trimPolylineOpen(gs,trimLine,tp)._2 should equal(Some(List(Vector2D(-20.0,00.0), Vector2D(-20.0,20.0), Vector2D(-10.0,20.0))))
    }
  }
  */

  //TODO: implement trimming of closed polylines
  describe("closed polylines... ") {

    it("should find the correct two intersections on a closed PL intersecting a line twice, regardless of draw order and segment ID locadion relative to the trim point") {

      /*
       END x    0
           *----*A---*
          D*         |1
          -*---------*--- GuideLine
          3|         *B
           *---*C----*
                2


      */

      val trimLine = PolylineShape(Rectangle2D(Vector2D(-20,-20),Vector2D(20,20)))

      //guideShape on same segment as trimPoint
      val gs1 = List(LineShape(Vector2D(-30,0),Vector2D(30,0)))

      //guideShape NOT on same segment as trimPoint
      val gs2 = List(LineShape(Vector2D(0,-30),Vector2D(0,30)))

      val tp = Vector2D(-20,10)

      val ints1 = TrimmingMethods.getIntersectSegmentNumbers(gs1,trimLine)
      val ints2 = TrimmingMethods.getIntersectSegmentNumbers(gs2,trimLine)

      //TrimmingMethods.findIntersectionClosed(trimLine,ints1,3,true,Some(tp)) should equal(Some((1,Vector2D(20.0,0.0)))) //positive direction
      //TrimmingMethods.findIntersectionClosed(trimLine,ints1,3,false,Some(tp)) should equal(Some((3,Vector2D(-20.0,0.0)))) //negative direction

      /*

           x 0  |
           *----*I---*
         TP*    |    |1
           |    |    |
          3|    |    |
           *----*I---*
                |  2

       */

      //direction END - should flip from 3 to 0 and find the top intersection.
      //TrimmingMethods.findIntersectionClosed(trimLine,ints2,3,true,Some(tp)) should equal(Some((0,Vector2D(0.0,20.0)))) //positive direction
      //direction START - should descend from 3 to 2 and find the bottom intersection.
      //TrimmingMethods.findIntersectionClosed(trimLine,ints2,3,false,Some(tp)) should equal(Some((2,Vector2D(0.0,-20.0)))) //negative direction

    }


    it("should not be trimmed when there is only one intersection") {
      val trimLine = PolylineShape(Rectangle2D(Vector2D(-20,-20),Vector2D(20,20)))
      val gs = Map(-4 -> PolylineShapeOpen(Vector2D(0,0),List(PolylineLineShape(Vector2D(0,30))),Attributes()))
      val tp = Vector2D(20,10)

      //TrimmingMethods.trimPolylineClosed(gs,trimLine,tp) should equal(None)
    }
    it("should be trimmed with two intersections") {

      val trimLine = PolylineShape(Rectangle2D(Vector2D(-20,-20),Vector2D(20,20)))
      val gs = Map(4 -> PolylineShapeOpen(Vector2D(-30,0),List(PolylineLineShape(Vector2D(30,0))),Attributes()))

      val tp = Vector2D(-20,10)
      val tp2 = Vector2D(-20,0)

      /*
  END x    0
      *---------*
    TP*         |1
     -*---------*--- GuideLine
     3|         |
      *---------*
           2
     */

      TrimmingMethods.trimPolylineClosed(gs,trimLine,tp) should equal(Some(List(Vector2D(0.0,-20.0), Vector2D(-20.0,-20.0), Vector2D(-20.0,20.0),Vector2D(0.0,20))))
      //TrimmingMethods.trimPolylineClosed(gs,trimLine,tp2) should equal(Some(List(Vector2D(0.0,20.0), Vector2D(20.0,-20.0), Vector2D(20.0,20.0),Vector2D(0.0,20))))
    }
  }
}