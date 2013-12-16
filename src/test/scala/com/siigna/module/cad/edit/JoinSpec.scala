/*
 * Copyright (c) 2008-2013, Selftie Software. Siigna is released under the
 * creative common license by-nc-sa. You are free
 *   to Share — to copy, distribute and transmit the work,
 *   to Remix — to adapt the work
 *
 * Under the following conditions:
 *   Attribution —   You must attribute the work to http://siigna.com in
 *                    the manner specified by the author or licensor (but
 *                    not in any way that suggests that they endorse you
 *                    or your use of the work).
 *   Noncommercial — You may not use this work for commercial purposes.
 *   Share Alike   — If you alter, transform, or build upon this work, you
 *                    may distribute the resulting work only under the
 *                    same or similar license to this one.
 *
 * Read more at http://siigna.com and https://github.com/siigna/main
 */

package com.siigna.module.cad.edit

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import com.siigna.util.geom.Vector2D
import com.siigna.app.model.shape.{PolylineLineShape, PolylineShape, LineShape}
import com.siigna.module.cad.joinMethods
import com.siigna.app.model.shape.PolylineShape.PolylineShapeOpen
import com.siigna.util.collection.Attributes

class JoinSpec extends FunSpec with ShouldMatchers  {

  val join = new Join

  describe("joining of shapes... ") {
    it("works with two shapes") {
      val p1 = Vector2D(0,0)
      val p2 = Vector2D(10,0)
      val p3 = Vector2D(10,10)
      val p4 = Vector2D(9,10)
      val s1 = LineShape(p1,p2)
      val s2 = LineShape(p4,p3)
      val l1 = List(p1,p2)
      val l2 = List(p2,p3)

      //joinMethods.endsCheck(s1,s2) should equal (false)
      //joinMethods.addTwoVerticeLists(l1,l2) should equal (List(Vector2D(0.0,0.0), Vector2D(10.0,0.0), Vector2D(10.0,0.0), Vector2D(10.0,10.0)))
    }

    it("works with more than two shapes") {
      val p1 = Vector2D(0,10)
      val p2 = Vector2D(0,0)
      val p3 = Vector2D(10,0)
      val p4 = Vector2D(10,10)

      val p5 = Vector2D(20,20)
      val p6 = Vector2D(30,20)
      val p7 = Vector2D(-10,20)
      val p8 = Vector2D(-10,30)


      val pl1 = PolylineShape(p1,p2,p3,p4)
      val pl2 = PolylineShape(p5,p6,p4)
      val pl3 = PolylineShape(p7,p8)
      val pl4 = PolylineShape(p7,p1)

      val selection1 = Map(1 -> pl1,2 -> pl2, 3 -> pl3)

      joinMethods.joinMultiple(selection1) should equal (List(PolylineShapeOpen(Vector2D(0.0,10.0),List(PolylineLineShape(Vector2D(0.0,0.0)), PolylineLineShape(Vector2D(10.0,0.0)), PolylineLineShape(Vector2D(10.0,10.0)), PolylineLineShape(Vector2D(30.0,20.0)), PolylineLineShape(Vector2D(20.0,20.0))), Attributes()), PolylineShapeOpen(Vector2D(-10.0,20.0),List(PolylineLineShape(Vector2D(-10.0,30.0))), Attributes())))

      val selection2 = Map(1 -> pl1,2 -> pl4, 3 -> pl2)
      //TODO: yields two shapes, not one?
      joinMethods.joinMultiple(selection2) should equal (List(PolylineShapeOpen(Vector2D(-10.0,20.0),List(PolylineLineShape(Vector2D(0.0,10.0)), PolylineLineShape(Vector2D(0.0,0.0)), PolylineLineShape(Vector2D(10.0,0.0)), PolylineLineShape(Vector2D(10.0,10.0)), PolylineLineShape(Vector2D(30.0,20.0)), PolylineLineShape(Vector2D(20.0,20.0))), Attributes())))


    }
  }
}
