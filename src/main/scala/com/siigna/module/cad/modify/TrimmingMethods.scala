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

import com.siigna.util.geom._
import com.siigna.app.model.shape._
import com.siigna.util.collection.Attributes
import com.siigna.app.Siigna

object TrimmingMethods {

  val trimGuideShapes : List[Shape] = List()

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

  /*
  Input:
  g : a list of guide shapes
  t : a shape to be trimmed

  Returns:
  a) the segment numbers (Int) on the trimShape on which an intersection occurs
  b) the intersection point
  */

  def getIntersectSegmentNumbers(g : List[PolylineShape], t : PolylineShape) : Map[Int,List[Vector2D]] = {
    //make a list of all intersections between the guideShapes and the trimShape
    val shapes = g.map(p => p.shapes).flatten.map(_.geometry)
    //1 = shape
    //2 = ID
    //make a list of tuples: the segment nr at which the intersection takes place, and the coordinate.
    val intersections = t.shapes.map(_.geometry).zipWithIndex.map(t => t._2 -> shapes.map(_.intersections(t._1)).flatten)
    intersections.toMap //return
  }
  //returns the segment (LineShape) and ID at given point.
  def findIntSegNrAtPoint(pl : PolylineShape, p : Vector2D) : Option[(Int, InnerPolylineShape)] = {
    //get the first segment within selection distance to p.
    val closest = pl.shapes.zipWithIndex.find(_._1.distanceTo(p) < Siigna.selectionDistance)
    print("CLOSEST: "+Siigna.selectionDistance)
    //return the ID and the innerShape (the two endpoints) of that segment TODO: .map innerShapes filters out one of the endpoints?
    //closest.map(t => t._2 -> pl.innerShapes(t._2))
    val cM = closest.map(t => t._2 -> pl.innerShapes(t._2))
    println("cM: "+cM)
    cM
  }

  /*calculate the relevant trim point(s) on a polyline
  input:
  1: the shape to be trimmed
  2: intSegNr: the segment number on which the trim point lies
  3: the list of all intersection vectors and corresponding ID's for the polyline to be trimmed (from getIntersectSegmentNumbers)
  4: a boolean telling if the intersections towards the start or the end of the polyline should be returned
  5: the trim point
  */
  def findIntersection(tL : PolylineShape, intSegNr : Int, i : Map[Int,List[Vector2D]], d : Boolean, p : Vector2D) : Option[Vector2D] = {

    //        *         trimSegment                           *
    //               i   intSegmentVectors           i    i
    //--|-----*------|------ p ----------------------|----|---*----
    //    endPoint      <-- direction                      segment endPt

    // get the ID for the segment on which p lies.
    val (trimSegmentInt, shape) = findIntSegNrAtPoint(tL, p).get

    //find the endpoint of the segment on which p lies .
    //if it is the first segment of the  trimline, the startpoint of the polyline is used.
    val endPoint = if(d) shape.point else if(trimSegmentInt == 0) tL.startPoint else tL.innerShapes(trimSegmentInt - 1).point

    //get intersecting vectors at the same segment as the segment p is on
    val intSegmentVectors = i(trimSegmentInt)

    //get intersecting points on the segment in the given direction (set with a boolean value), if any:
    val r = intSegmentVectors.filter(_.distanceTo(p)<p.distanceTo(endPoint))
    println("r: "+r)
    r.size match{
      //evaluate adjacent segments to look for intersections there.
      case(0) => {
        //check if there are more segments on the polyline.
        //if so, iterate through the segments, starting with the ones closest to the intSegment.
        //check if there are intersections.
        //If there are more than one int on a segment, return the one closest the endPoint closest to the intSegment.
        println("test adjacent segments here!")
        None
      }
      //if there is just one intersection, we know that the polyline should be trimmed by that point, so it is returned.
      case(1) => {
        println("return intersection vector here")
        None
      }
      //if there are more than one point, the one closest to p should be returned.
      case _  => {
        r.reduceLeft((a,b) => if(a.distanceTo(p) < b.distanceTo(p)) a else b)
        None
      }
    }


  }




  /* getSubSegment:
        *----
        | -> second subsegment
        * (d)
       (p)
        | -> first subsegment
     ---*
   input:
   p1, p2 (segment)
   division point d
   evaluation point p

   evaluates if a point (p) lies on the first or second sub segment of a line (p1,p2)
   -which has been divided by a point (d).
   Returns 0 if the point is not on any of the segments
   Returns 1 if the point is on the first subsegment.
   Returns 2 if the point is on the second subsegment.
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



  //trim Polylines
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

  def trimPolyline(guides : List[PolylineShape], trimLine : PolylineShape, p : Vector2D) : List[Shape] = {
    var trimmedLine1 = List[Vector2D]()
    var trimmedLine2 = List[Vector2D]()

    //find the segments adjacent to the trimpoint on which there are intersections.
    val intSegment = getIntersectSegmentNumbers(guides,trimLine) //find segment numbers for intersection. ._2: segment nr. of the trimShape

    println("intersection segment: "+intSegment)

    //return
    List()
  }
}
