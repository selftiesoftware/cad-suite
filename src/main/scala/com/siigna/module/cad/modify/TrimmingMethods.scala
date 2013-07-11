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

  def getIntersectSegmentNumbers(g : List[Shape], t : PolylineShape) : Map[Int,List[Vector2D]] = {
    //make a list of all intersections between the guideShapes and the trimShape
    val shapes = g.map(_.geometry)
    //1 = shape
    //2 = ID
    //make a list of tuples: the segment nr at which the intersection takes place, and the coordinate.
    val intersections = t.shapes.map(_.geometry).zipWithIndex.map(t => t._2 -> shapes.map(_.intersections(t._1)).flatten)
    intersections.toMap //return
  }
  //returns the segment (LineShape) and ID at given point.
  def findIntSegNrAtPoint(pl : PolylineShape, p : Vector2D) : Option[(Int, GeometryBasic2D)] = {
    //get the first segment within selection distance to p.
    val closest = pl.shapes.zipWithIndex.find(_._1.distanceTo(p) < Siigna.selectionDistance)
    //return the ID and the innerShape (the two endpoints) of that segment TODO: .map innerShapes filters out one of the endpoints?
    //closest.map(t => t._2 -> pl.innerShapes(t._2))
    val cM = closest.map(t => t._2 -> pl.shapes(t._2).geometry)

    if(cM.isDefined) cM else None
  }

  /* calculate the relevant trim point on a polyline in a given direction.
  Note: This means the function should be run twice to calculate trimming points in both directions from p.

  input:
  1: the shape to be trimmed
  2: the list of all intersection vectors and corresponding ID's for the polyline to be trimmed (from getIntersectSegmentNumbers)
  3: a boolean telling if the intersections towards the start or the end of the polyline should be returned
  4: the trim point

  returns: The Vector2D at which the Polyline should be trimmed, or None.
  */
  def findIntersection(tL : PolylineShape, i : Map[Int,List[Vector2D]], d : Boolean, p : Vector2D) : Option[Vector2D] = {

    //        *         trimSegment                           *
    //               i   intSegmentVectors           i    i
    //--|-----*------|------ p ----------------------|----|---*----
    //    endPoint      <-- (d)irection                 segment endPoint

    // get the ID for the segment on which p lies.   OK
    val (trimSegmentInt, shape) = findIntSegNrAtPoint(tL, p).get

    //find the endpoint of the segment on which p lies .
    //if it is the first segment of the  trimline, the startpoint of the polyline is used.
    val endPoint = if(d) shape.vertices(1) else if(trimSegmentInt == 0) tL.startPoint else shape.vertices(0)

    //get intersecting vectors at the same segment as the segment p is on   OK
    val intSegmentVectors = i(trimSegmentInt)

    //get intersecting points on the segment in the given direction E1 or E2 (set with a boolean value), if any:
    //based on a distance calculation: filters out ints with a distance greater than d(x,E2)
    //  E1 |---A--- p --- B----| E2
    //         |<  d(A,E2)    >|
    //              |< d(x,E2)-|

    val r = intSegmentVectors.filter(_.distanceTo(endPoint)<p.distanceTo(endPoint))
    r.size match{
      //if there are no intersections on the segment on which p lies...
      case(0) => {
        //iterate through next segments, starting with the ones closest to the intSegment.
        //in the given direction.
        if(d){
          //and check for intersections
          println("going through higher segments")


        } else {
          println("going through lower segments")
        }


        //check if there are intersections.

        //If there are more than one int on a segment, return the one closest to the endPoint which is closest to the trimpoint.

        None
      }
      //if there is just one intersection, we know that the polyline should be trimmed by that point, so it is returned.
      case(1) => {
        Some(r.head)
      }
      //if there are more than one point, the one closest to p should be returned.
      case _  => {
        Some(r.reduceLeft((a,b) => if(a.distanceTo(p) < b.distanceTo(p)) a else b))
      }
    }
  }

  //a function that splits a shape into two lists by a point
  def splitPolyline(p : List[Vector2D], splitPoint : Vector2D) : (List[Vector2D], List[Vector2D]) = {
    var list1 = List[Vector2D]()
    var list2 = List[Vector2D]()
    var r = 0
    //evaluate on which segment the splitpoint lies -unless the length is two, in which case it is not a polyline
    if(p.length >= 2) for(i <- 0 to (p.length - 2)) if(IsOnSegment(p(i),p(i+1),splitPoint)) r = i

    for (i <- 0 to r) list1 = list1 :+ p(i) //make a list of points up until the split segment
    for (i <- (r + 1) to p.length - 2) list2 = list2 :+ p(i) //make a list of points after the split segment
    (list2, list1)  //return the two lists
  }

  /*
 * a function to trim a polylineShape
 * gs = the trimGuideShape(s)
 * ts = the shape to be trimmed
 * p = trim point (the part of ts which should be deleted
 */

  def trimPolyline(guides : Map[Int,Shape], trimLine : Shape, p : Vector2D) : List[Shape] = {
    val trimmedLine1 : Option[List[Vector2D]] = None
    val trimmedLine2 : Option[List[Vector2D]] = None

    //TODO: check that the trimLine IS a polyline!
    val intIDs = getIntersectSegmentNumbers(guides.map(_._2).toList,trimLine.asInstanceOf[PolylineShape])

    //find intersections in the positive direction
    val int1 = findIntersection(trimLine.asInstanceOf[PolylineShape],intIDs,true,p)

    //find intersections in the negative direction
    val int2 = findIntersection(trimLine.asInstanceOf[PolylineShape],intIDs,false,p)

    println("GOT FIRST INTERSECTION! : "+int1)
    println("GOT SECOND INTERSECTION! : "+int2)

    //return
    List()
  }
}
