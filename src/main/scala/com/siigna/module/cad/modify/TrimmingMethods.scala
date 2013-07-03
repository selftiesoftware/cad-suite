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

package com.siigna.module.cad.modify

import com.siigna._
import com.siigna.util.geom.Geometry2D
import scala.Array

object TrimmingMethods {


  val trimGuideShapes : List[Shape] = List()
  private var trimGuide : Option[Shape] = None
  private var trimGuide2 : Option[Shape] = None
  private var trimID : Option[Int] = None
  private var trimID2 : Option[Int] = None

  def IsOnSegment(pt1 : Vector2D, pt2 : Vector2D, pt3 : Vector2D) = {
    val xi = pt1.x
    val yi = pt1.y
    val xj = pt2.x
    val yj = pt2.y
    val xk = pt3.x
    val yk = pt3.y
    if((xi <= xk || xj <= xk) && (xk <= xi || xk <= xj) && (yi <= yk || yj <= yk) && (yk <= yi || yk <= yj)) {
      true
    } else false
  }

  def ComputeDirection(pt1 : Vector2D, pt2 : Vector2D, pt3 : Vector2D) = {
    val xi = pt1.x
    val yi = pt1.y
    val xj = pt2.x
    val yj = pt2.y
    val xk = pt3.x
    val yk = pt3.y
    val a = (xk - xi) * (yj - yi)
    val b = (xj - xi) * (yk - yi)
    //return (a < b) ? -1 : a > b ? 1 : 0
    if(a < b) -1
    else if(a > b) 1
    else 0
  }

  //getIntersections. Returns the segment number (Int) of the intersection for the respective input shapes:
  //the trimguide, and the trimshapes, respectively
  def getIntersectSegmentNumber(g : Geometry2D, t : Geometry2D) : (List[Int],List[Int]) = {
    val gV = g.vertices
    val tV = t.vertices
    var gS = List[Int]()
    var tS = List[Int]()
    if(gV.length >= 2 && tV.length >= 2) {
      for (i <- 0 to tV.length - 2) {
        for(j <- 0 to gV.length - 2) {
          if(g.intersects(t) == true){

            gS = gS :+ j
            tS = tS :+ i
          }
        }
      }
    }
    (gS, tS)
  }

  /* getSubSegment:
        *----
        | -> second subsegment
        * (d)
       (p)
        | -> first subsegment
     ---*
   evaluate if a point (p) lies on the first or second sub segment of a line (p1,p2)
   -which has been divided by a point (d).
   Returns 0 if the point is on the first subsegment.
   Returns 1 if the point is not on any of the segments
   Returns 2 if the point is on the first subsegment.
  */

  def getSubSegment (p1: Vector2D, p2 : Vector2D, d : Vector2D, p : Vector2D) : Int = {
    val dist1 = p.distanceTo(Segment2D(p1,d))
    val dist2 = p.distanceTo(Segment2D(p2,d))

    if(dist1 < dist2) 1
    else if(dist1 > dist2) 2
    else 0
  }

  //a function that splits a shape into two lists by a point
  def splitPolyline(p : List[Vector2D], splitPoint : Vector2D) : (List[Vector2D], List[Vector2D]) = {
    var list1 = List[Vector2D]()
    var list2 = List[Vector2D]()
    var r = 0
    //evaluate on which segment the splitpoint lies -unless the length is two, in which case it is not a polyline
    if(p.length >= 2) for(i <- 0 to (p.length - 2)) if(IsOnSegment(p(i),p(i+1),splitPoint) == true) r = i

    for (i <- 0 to r) list1 = list1 :+ p(i) //make a list of points up until the split segment
    for (i <- (r + 1) to p.length - 2) list2 = list2 :+ p(i) //make a list of points after the split segment
    (list2, list1)  //return the two lists
  }

  //evaluates a guideline and a (poly)line returns the polyline, trimmed by the line.
  //used for situation B
  //TODO: allow trimming of lines that intersect the guide multiple times
  def trimBetween(guide : Geometry2D, trimLine : Geometry2D, p : Vector2D) : (List[Vector2D], List[Vector2D]) = {
    var trimV1 = List[Vector2D]()
    var trimV2 = List[Vector2D]()
    val g : Seq[Vector2D] = guide.vertices
    val t : Seq[Vector2D] = trimLine.vertices
    val intSegment = getIntersectSegmentNumber(guide,trimLine) //find segment numbers for intersections
    val intSegment1 : List[Int] = getIntersectSegmentNumber(guide,trimLine)._1 //find segment numbers for intersections
    val intSegment2 : List[Int] = getIntersectSegmentNumber(guide,trimLine)._2 //find segment numbers for intersections

    //store return values for the first intersection;
    val guideSegmentNr1 = intSegment1(0)
    val guideSegmentNr2 = intSegment1(1)
    val trimSegmentNr1 = intSegment2(0)
    val trimSegmentNr2 = intSegment2(1)

    println("guideSegmentNr1: "+guideSegmentNr1)
    println("guideSegmentNr2: "+guideSegmentNr2)
    println("trimSegmentNr1: "+trimSegmentNr1)
    println("trimSegmentNr2: "+trimSegmentNr2)

    val guideSegment1 = Line2D(g(guideSegmentNr1),g(guideSegmentNr1+1))
    val guideSegment2 = Line2D(g(guideSegmentNr2),g(guideSegmentNr2+1))
    val trimSegment1 = Line2D(t(trimSegmentNr1),t(trimSegmentNr1+1))
    val trimSegment2 = Line2D(t(trimSegmentNr2),t(trimSegmentNr2+1))
    val int1 = guideSegment1.intersections(trimSegment1)   //do the intersection!
    val int2 = guideSegment2.intersections(trimSegment2)   //do the intersection!


    //one or first intersection - get the trimline
    if (!int1.isEmpty) {
      val getSegment = (getSubSegment(t(trimSegmentNr1),t(trimSegmentNr1+1),int1.head,p))
      val twoLists = splitPolyline(t.toList,int1.head)//the trimline split into two lists

      if(getSegment == 1) { //if the trim point is set before the trimline, keep the first segments.
        trimV1 = (twoLists._1 :+ t.last).reverse :+ int1.head
      }
      else if(getSegment == 2) { //if the trim point is set after the trimline, keep the last segments.
        trimV1 = twoLists._2 :+ int1.head
      } else trimV1 = trimLine.vertices.toList
    }
    //if two intersections between the trimline and guideline are found, get the second trimline:
    if (!int2.isEmpty) {
      val getSegment = (getSubSegment(t(trimSegmentNr2),t(trimSegmentNr2+1),int2.last,p))
      val twoLists = splitPolyline(t.toList,int2.head)//the trimline split into two lists

      if(getSegment == 1) { //if the trim point is set before the trimline, keep the first segments.
        trimV2 = (twoLists._1 :+ t.last).reverse :+ int2.head
      }
      else if(getSegment == 2) { //if the trim point is set after the trimline, keep the last segments.
        trimV2 = twoLists._2 :+ int2.head
      } else trimV2 = trimLine.vertices.toList
    }
    //else trimV = t.toList //if not intersection is found, return the original trimLine
    (trimV1, trimV2) //return the trimmed line
  }
  //used for situations A and C
  def trimEnd(guide : Geometry2D, trimLine : Geometry2D, p : Vector2D) : List[Vector2D] = {
    var trimV1 = List[Vector2D]()
    var trimV2 = List[Vector2D]()
    val g : Seq[Vector2D] = guide.vertices
    val t : Seq[Vector2D] = trimLine.vertices
    val intSegment = getIntersectSegmentNumber(guide,trimLine) //find segment numbers for intersection

    //store return values for the intersection;
    /*val guideSegmentNr = intSegment(0)
    val trimSegmentNr = intSegment(0)

    println("guideSegmentNr: "+guideSegmentNr)
    println("trimSegmentNr: "+trimSegmentNr)

    val guideSegment = Line2D(g(guideSegmentNr),g(guideSegmentNr+1))
    val trimSegment = Line2D(t(trimSegmentNr),t(trimSegmentNr+1))
    val int = guideSegment.intersections(trimSegment)   //do the intersection!


    //one or first intersection - get the trimline
    val getSegment = (getSubSegment(t(trimSegmentNr),t(trimSegmentNr+1),int.head,p))
    val twoLists = splitPolyline(t.toList,int1.head)//the trimline split into two lists

    if(getSegment == 1) { //if the trim point is set before the trimline, keep the first segments.
      trimV1 = (twoLists._1 :+ t.last).reverse :+ int1.head
    }
    else if(getSegment == 2) { //if the trim point is set after the trimline, keep the last segments.
      trimV1 = twoLists._2 :+ int1.head
  } else trimV1 = trimLine.vertices.toList


    //else trimV = t.toList //if not intersection is found, return the original trimLine
    */
    //Array(PolylineShape(trimV1))
    List(Vector2D(0,0),Vector2D(10,0))

    //PolylineShape( //return the trimmed line
  }

  /*
   * a function to trim a polylineShape
   * gs = the trimGuideShape(s)
   * ts = the shape to be trimmed
   * p = trim point (the part of ts which should be deleted
   *
   *  SITUATION A - one trimGuideShape
   *
   *            gs
   *            |            ts
   *      *-----|------ * p ----------*
   *            |
   *
   *  SITUATION B - two or more trimGuideShape, p between two of them
   *
   *            gs        gs
   *            |          |   ts
   *      *-----|--- * p --|------*
   *            |          |
   *
   * SITUATION C - two or more trimGuideShape, p not between two of them
   *
   *            gs        gs
   *            |          |        ts
   *      *-----|----------|-- * p ----*
   *            |          |
   */

  def trimTwoPolyLineShapes(gs : List[PolylineShape], ts : PolylineShape, p : Vector2D, attr : Attributes) = {
    //store intersections
    var ints = List[Set[Vector2D]]()
    var intersectingShapes = List[PolylineShape]()

    gs.foreach(g => {
      //get the intersections
      if (g.geometry.intersects(ts.geometry)) {
        ints = g.geometry.intersections(ts.geometry) :: ints
        intersectingShapes = g :: intersectingShapes

      } else println("NO INTERSECTION")
    })
    //situation A or C:
    if(ints.length == 1) {
      //return the new shape
      val newShape = trimEnd(intersectingShapes(0).geometry,ts.geometry,p)
      //and create it:
      //Create(newShape)
    }
    //situation B:
    else if (ints.length > 1) {
      println("more than one trimPoints")
    }
    else None
  }
}
