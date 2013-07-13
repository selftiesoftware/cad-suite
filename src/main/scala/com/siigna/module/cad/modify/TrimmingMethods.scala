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
import com.siigna.app.Siigna

object TrimmingMethods {

  /**
   * Finds the segment number on which there is an intersection, if any.
   * Input:
   * g : a list of guide shapes
   * t : a shape to be trimmed
   *
   * Returns:
   * a) the segment numbers (Int) on the trimShape on which an intersection occurs
   * b) the intersection point
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

  /**
   * Returns the segment (LineShape) and ID of the closest segment to a point.
   */
  def findIntSegNrAtPoint(pl : PolylineShape, p : Vector2D) : Option[(Int, GeometryBasic2D)] = {
    //get the first segment within selection distance to p.
    val closest = pl.shapes.zipWithIndex.find(_._1.distanceTo(p) < Siigna.selectionDistance)
    //return the ID and the innerShape (the two endpoints) of that segment TODO: .map innerShapes filters out one of the endpoints?
    val cM = closest.map(t => t._2 -> pl.shapes(t._2).geometry)

    if(cM.isDefined) cM else None
  }

  /* calculate the relevant trim point on a polyline in a given direction.
  Note: This means the function should be run twice to calculate trimming points in both directions from p.

  input:
  1: the shape to be trimmed
  2: the list of all intersection vectors and corresponding ID's for the polyline to be trimmed (from getIntersectSegmentNumbers)
  3: the ID of the segment on which the trim point is set (with MouseDown)
  4: a boolean telling if the intersections towards the start or the end of the polyline should be returned
  5: the trim point

  returns: Vector2Ds which are needed to construct the trimmed Polyline, or None.
  */

  def findIntersection(tL : PolylineShape, intIDs : Map[Int, List[Vector2D]], id : Int, d : Boolean, p : Option[Vector2D]) : Option[(Int, Vector2D)] = {
    //get intersecting vectors at the same segment as the segment p is on   OK

    val intersections = intIDs(id)
    val shape = tL.shapes(id)

    //find the endpoint of the segment on which p lies .
    //if it is the first segment of the  trimline, the startpoint of the polyline is used.
    val endPoint = if(d) shape.geometry.vertices(0) else if(id == 0) tL.startPoint else shape.geometry.vertices(1)

    //get intersecting points on the segment in the given direction E1 or E2 (set with a boolean value), if any:
    //based on a distance calculation: filters out ints with a distance greater than d(x,E2)
    //  E1 |---A--- p --- B----| E2
    //         |<  d(A,E2)    >|
    //              |< d(x,E2)-|


    //test if there are intersections on the first segment that is being evaluated (there is a trim point, case Some(x)).
    //if so, store them in the val r, and filter the intersections to get the ones on the right side of the trim point only:
    val r = p match {
      case Some(x) => {
        val p = intersections.filter(_.distanceTo(endPoint)>x.distanceTo(endPoint))
        println("P: "+p)
        p
      }
      case _ => intersections
    }
    r.size match{
      //if there are no intersections on the segment on which p lies...
      case(0) => {
        //iterate through next segments, starting with the ones closest to the intSegment.
        //in the given direction.

        if(d){
          id match {
            case i if i >= tL.size - 1 => {
              None
            }
            case _ => {
              findIntersection(tL, intIDs, (id + 1), true, None)
            }
          }
        } else {
          id match {
            case i if i <= 0 => {
              None
            }
            case _ => {
              findIntersection(tL, intIDs, id - 1, false, None)
            }
          }
        }
      }

      //if there is just one intersection, we know that the polyline should be trimmed by that point, so it is returned.
      case(1) => {
        Some(id -> r.head)
      }
      //if there are more than one point, the one closest to p should be returned.
      case _  => {
        val x = p match {
          // if there is a mouse point, use it
          case Some(x) => x
          //if not, use the end point:
          case _ => endPoint
        }
        Some(id -> r.reduceLeft((a,b) => if(a.distanceTo(x) < b.distanceTo(x)) a else b))
      }
    }
  }

  /*
  a function to trim a polylineShape

  input:
  gs = the trimGuideShape(s)
  ts = the shape to be trimmed
  p = trim point (the part of ts which should be deleted

  returns:
  a list of Option[Shape] (because one or both trim lines may or may not exist)
  */

  def trimPolyline(guides : Map[Int,Shape], shape : Shape, p : Vector2D) : (Option[List[Vector2D]],Option[List[Vector2D]]) = {
    var t1 : Option[List[Vector2D]] = None
    var t2 : Option[List[Vector2D]] = None
    //TODO: allow trimming of LineShape types (and arcs and circles...)
    val trimLine = shape.asInstanceOf[PolylineShape]
    val trimVertices = trimLine.geometry.vertices.toList

    //TODO: check that the trimLine IS a polyline!
    val intIDs = getIntersectSegmentNumbers(guides.map(_._2).toList,trimLine)

    // Where does the mouse intersect the PL?
    //        *         trimSegment                           *
    //               i   intSegmentVectors           i    i
    //--|-----*------|------ p ----------------------|----|---*----
    //    endPoint      <-- (d)irection (true/false)      segment endPoint

    // get the ID for the segment on which p lies.   OK
    val (trimSegmentInt, _) = findIntSegNrAtPoint(trimLine, p).get

    //find intersections in the positive direction.
    //construct and return the first trimline, if any
    val line1 = findIntersection(trimLine, intIDs, trimSegmentInt, true, Some(p)) match {
      case Some((id1, int1)) => {
        // remove the trimmed vertices, but add the intersection vertex:
        Some(trimVertices.drop(id1 + 1).+:(int1))
      }
      case _ => None
    }

    //find intersections in the negative direction
    //construct and return the first trimline, if any
    val line2 = findIntersection(trimLine, intIDs, trimSegmentInt, false, Some(p)) match {
      case Some((id2, int2)) => {
        // remove the trimmed vertices, but add the intersection vertex:
        Some(trimVertices.take(id2 + 1) :+ int2)
      }
      case _ => None
    }
    //return
    (line1,line2)
  }
}
