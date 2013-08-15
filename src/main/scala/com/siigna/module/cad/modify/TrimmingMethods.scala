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
import com.siigna.app.model.shape.PolylineShape.PolylineShapeClosed

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
    //println("T: " + t.geometry)
    //println("G: " + g(0).geometry)
    //println("INTS: "+(t.geometry).intersections(g(0).geometry))

    //make a list of tuples: the segment nr at which the intersection takes place, and the coordinate.
    val intersections = t.shapes.map(_.geometry).zipWithIndex.map(t => t._2 -> shapes.map(_.intersections(t._1)).flatten)
    val i = intersections.toMap //return
    i
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

  /* calculate the relevant trim point on a closed polyline in a given direction.
  Note: This means the function should be run twice to calculate trimming points in both directions from p.

  Return:
  should always return one trimmed polyline or none. Never two polylines

  input:
  1: the shape to be trimmed
  2: the list of all intersection vectors and corresponding ID's for the polyline to be trimmed (from getIntersectSegmentNumbers)
  3: the ID of the segment on which the trim point is set (with MouseDown)
  4: d: a boolean telling if the intersections towards the start (TRUE) or the end (FALSE) of the polyline should be returned
  5: p: the trim point

  returns: Vector2Ds which are needed to construct the trimmed Polyline, or None.
  */

  def findIntersectionClosed(tL : PolylineShape, intIDs : Map[Int, List[Vector2D]], i : Int, d : Boolean, p : Option[Vector2D]) : Option[(Int, Vector2D)] = {
    //make the id variable, in order to allow evaluating ints twice (to continue from the end segment of the closed PL=
    var id = i
    //get intersecting vectors at the same segment as the segment p is on   OK
    val intersections = intIDs(id)
    val shape = tL.shapes(id)

    //find the endpoint of the segment on which p lies .
    //if the direction is FALSE, the startpoint of the polyline is used.   GOING TOWARDS START
    //if the direction is TRUE, the startPoint + 1 (next point) is used.   GOING TOWARDS END
    val endPoint = if(d) {
      shape.geometry.vertices(0)
    } else {
      shape.geometry.vertices(1)
    }

    //a flag to establish if the segments have been evaluated once:
    var firstRunComplete = false

    //a flag to prevent an endless loop:
    var secondIteration = false

    //get the highest ID value
    var maxID = tL.size - 1

    //get intersecting points on the segment in the given direction E1 or E2 (set with a boolean value), if any:
    //based on a distance calculation: filters out ints with a distance greater than d(x,E2)
    //  E1 |---A--- p --- B----| E2
    //         |<  d(A,E2)    >|
    //              |< d(x,E2)-|


    //test if there are intersections on the first segment that is being evaluated (there is a trim point, case Some(x)).
    //if so, store them in the val r, and filter the intersections to get the ones on the right side of the trim point only:
    val r = p match {
      case Some(x) => {
        val p = if(d) {
          intersections.filter(_.distanceTo(endPoint)>x.distanceTo(endPoint))
        } else {
          intersections.filter(_.distanceTo(endPoint)>x.distanceTo(endPoint))
        }
        p
      }
      case _ => intersections
    }
    r.size match{
      //if there are no intersections on the segment on which p lies...
      case(0) => {
        //iterate through next segments, starting with the ones closest to the intSegment.
        //in the given direction.

        //if TRUE (going towards the END of the PL)  - then evaluating from START and down.
        if(d){
          id match {
            case i if i >= maxID => {
              /*since the pl is closed:
              if the end of the line is reached, the evaluation should continue to see if ints exist going from the other end.
              we've reached the end of the line twice, and found no intersections.

                schematic of a closed polyline to be trimmed:

                A,B,C,D : possible trimPoints
                0,1,2,3 : segment numbers

                END x->  0  *A
                   *---------*
                D* |         |1
                 --*---------*--- GuideLine
                  3|         |*B
                   *---------*
                     *C 2

              */

              //the end is reached, time to try in the same direction, but from the other end:
              firstRunComplete = true

              //reset eval id
              id = - 1
              //run again
              findIntersectionClosed(tL, intIDs, (id + 1), true, None)
            }
            //goto next segment
            case _ => {
              //if the start is reached, reset the ID to ru again from the end of the PL:
              if(firstRunComplete && id != 0) {
                findIntersectionClosed(tL, intIDs, (id + 1), true, None)
              } else if(firstRunComplete && id == 0) None
              // if the end is reached a second time, the evaluation should stop:
              else findIntersectionClosed(tL, intIDs, (id + 1), true, None)
            }
          }
        //if FALSE ( going towards the START of the PL)
        } else {

          id match {
            case i if i <= 0 => {
              //the start is reached, time to try in the same direction, but from the end:
              firstRunComplete = true
              //end only if the second iteration has finished
              id = maxID
              findIntersectionClosed(tL, intIDs, (id), false, None)

            }
            case _ => {
              //if the end is reached, reset the ID to start from the beginning of the PL:
              if(firstRunComplete && id != maxID) {
                findIntersectionClosed(tL, intIDs, (id - 1), false, None)
              } else if(firstRunComplete && id == maxID) None
              // if the end is reached a second time, the evaluation should stop:
              else {
                findIntersectionClosed(tL, intIDs, (id - 1), false, None)
              }
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

  /* calculate the relevant trim point on an open polyline in a given direction.
  Note: This means the function should be run twice to calculate trimming points in both directions from p.

  input:
  1: the shape to be trimmed
  2: the list of all intersection vectors and corresponding ID's for the polyline to be trimmed (from getIntersectSegmentNumbers)
  3: the ID of the segment on which the trim point is set (with MouseDown)
  4: d: a boolean telling if the intersections towards the start (TRUE) or the end (FALSE) of the polyline should be returned
  5: p: the trim point

  returns: Vector2Ds which are needed to construct the trimmed Polyline, or None.
  */

  def findIntersectionOpen(tL : PolylineShape, intIDs : Map[Int, List[Vector2D]], id : Int, d : Boolean, p : Option[Vector2D]) : Option[(Int, Vector2D)] = {
    //get intersecting vectors at the same segment as the segment p is on   OK
    val intersections = intIDs(id)
    val shape = tL.shapes(id)

    //find the endpoint of the segment on which p lies .
    //if the direction is FALSE, the startpoint of the polyline is used. - EVALUATING TOWARDS START
    //if the direction is TRUE, the startPoint + 1 (next point) is used. - EVALUATING TOWARDS END
    val endPoint = if(d) {
      shape.geometry.vertices(0)
    } else {
      shape.geometry.vertices(1)
    }

    //get intersecting points on the segment in the given direction E1 or E2 (set with a boolean value), if any:
    //based on a distance calculation: filters out ints with a distance greater than d(x,E2)
    //  E1 |---A--- p --- B----| E2
    //         |<  d(A,E2)    >|
    //              |< d(x,E2)-|


    //test if there are intersections on the first segment that is being evaluated (there is a trim point, case Some(x)).
    //if so, store them in the val r, and filter the intersections to get the ones on the right side of the trim point only:
    val r = p match {
      case Some(x) => {
        val p = if(d) {
          intersections.filter(_.distanceTo(endPoint)>x.distanceTo(endPoint))
        } else {
          intersections.filter(_.distanceTo(endPoint)>x.distanceTo(endPoint))
        }
        p
      }
      case _ => intersections
    }
    r.size match{
      //if there are no intersections on the segment on which p lies...
      case(0) => {
        //iterate through next segments, starting with the ones closest to the intSegment.
        //in the given direction.

        //direction END
        if(d){
          id match {
            case i if i >= tL.size - 1 => {
              None
            }
            case _ => {
              findIntersectionOpen(tL, intIDs, (id + 1), true, None)
            }
          }
        //direction START
        } else {
          id match {
            case i if i <= 0 => {
              None
            }
            case _ => {
              findIntersectionOpen(tL, intIDs, id - 1, false, None)
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
  a function to trim a LineShape

  input:
  gs = the trimGuideShape(s)
  ts = the shape to be trimmed
  p = trim point (the part of ts which should be deleted)

  returns:
  two lists of Option[Segment2D] (because one or both trim lines may or may not exist)
  */

  def trimLine(guides : Map[Int,Shape], shape : Shape, p : Vector2D) : (Option[Segment2D],Option[Segment2D]) = {
    val trimLine = shape.asInstanceOf[LineShape]
    val startPoint = trimLine.start //get the startPoint of the LineShape
    val endPoint = trimLine.end //get the endPoint of the LineShape

    //make an empty list...
    var intersections = List[Vector2D]()

    //... and populate it with intersections between the guides and the line to trim.
    guides.foreach(g => {
      val pt = g._2.geometry.intersections(trimLine.geometry)
      if(!pt.isEmpty) intersections = intersections ::: pt.toList
    })

    //find the intersections which lie between the trimPoint and startPoint:
    val intsTowardsStart = intersections.filterNot(v => v.distanceTo(startPoint)>p.distanceTo(startPoint))

    //find the intersections which lie between the trimPoint and endPoint:
    val intsTowardsEnd = intersections.filterNot(v => v.distanceTo(endPoint)>p.distanceTo(endPoint))

    //DIR. START: find the intersection closest to the trim point, and create the relevant trimmed segment if any:
    val startSegment = {
      if(!intsTowardsStart.isEmpty) Some(Segment2D(startPoint,intsTowardsStart.reduceLeft((a,b) => if(a.distanceTo(p) < b.distanceTo(p)) a else b)))
      else None
    }

    //DIR. END: find the intersection closest to the trim point, and create the relevant trimmed segment if any:
    val endSegment = {
      if(!intsTowardsEnd.isEmpty) Some(Segment2D(endPoint,intsTowardsEnd.reduceLeft((a,b) => if(a.distanceTo(p) < b.distanceTo(p)) a else b)))
      else None
    }

    //return the trimmed line(s) as a new segment from Start/End to startInt/endInt:
    (startSegment,endSegment)
  }


  /*
  a function to trim an open polylineShape

  input:
  gs = the trimGuideShape(s)
  ts = the shape to be trimmed
  p = trim point (the part of ts which should be deleted)

  returns:
  two lists of Option[Shape] (because one or both trim lines may or may not exist)
  */

  def trimPolylineOpen(guides : Map[Int,Shape], shape : Shape, p : Vector2D) : (Option[List[Vector2D]],Option[List[Vector2D]]) = {

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
    val line1 = findIntersectionOpen(trimLine, intIDs, trimSegmentInt, true, Some(p)) match {
      case Some((id1, int1)) => {
        // remove the trimmed vertices, but add the intersection vertex:
        Some(trimVertices.drop(id1 + 1).+:(int1))
      }
      case _ => None
    }

    //find intersections in the negative direction
    //construct and return the first trimline, if any
    val line2 = findIntersectionOpen(trimLine, intIDs, trimSegmentInt, false, Some(p)) match {
      case Some((id2, int2)) => {
        // remove the trimmed vertices, but add the intersection vertex:
        Some(trimVertices.take(id2 + 1) :+ int2)
      }
      case _ => None
    }
    //return
    (line1,line2)
  }

  /*
  TRIMMING OF CLOSED POLYLINES
 */

  def trimPolylineClosed(guides : Map[Int,Shape], shape : Shape, p : Vector2D) : Option[List[(Int,Vector2D)]] = {

    val trimLine = shape.asInstanceOf[PolylineShapeClosed]

    //construct a map assigning an ID to each vertex. Used to find out on which segment the mouse is pressed -
    //which in turn allow the calculation of which line to keep and which to discard.

    val trimVertices = trimLine.geometry.vertices.toList.zipWithIndex.map(t => t._2 -> t._1)

    var trimmedLine : Option[List[(Int,Vector2D)]] = None

    val intIDs = getIntersectSegmentNumbers(guides.map(_._2).toList,trimLine)
    val ints = intIDs.count(!_._2.isEmpty)

    //if there are less than two intersections, the polyline should not be trimmed.
    if(ints > 1) {

      // get the ID for the segment on which p lies.
      val (trimSegmentInt, trimGeom) = findIntSegNrAtPoint(trimLine, p).get

      //find intersections in the positive direction.
      //construct and return the first trimline, if any
      val int1 = findIntersectionClosed(trimLine, intIDs, trimSegmentInt, true, Some(p))

      val id1 = int1.get._1

      //find intersections in the negative direction
      //construct and return the first trimline, if any
      val int2 = findIntersectionClosed(trimLine, intIDs, trimSegmentInt, false, Some(p))
      val id2 = int2.get._1

      //construct the line from START to INT1:
      //take: take the first elements until INT1. Then remove the first point (drop(1)), as it will be added with line 1B.

      val line1A = {
        if(id1<id2) (trimVertices.take(id1 + 1) :+ int1.get).drop(1)
        else (trimVertices.take(id2 + 1) :+ int2.get).drop(1)
      }

      println("line1A: "+line1A)

      //construct the line from INT2 to END:
            //drop: Selects all elements except first n ones
      val line1B = {
        if(id1<id2) trimVertices.drop(id2 + 1).+:(int2.get)
        else trimVertices.drop(id1 + 1).+:(int1.get)
      }

      //join the two parts into the first possible trimline to keep:
      val line1 = line1B ++ line1A

      //construct the line from INT1 to INT2: (taking into account that sometimes id1>id2

      val line2A = {
        if(id1<id2)trimVertices.slice(int1.get._1 + 1,int2.get._1 + 1)
        else trimVertices.slice(int2.get._1 + 1,int1.get._1 + 1)
      }
      // add the first trimPoint
      val line2B = {
        if(id1<id2)line2A.+:(int1.get)
        else line2A.+:(int2.get)
      }
      //add the second trimPoint
      val line2 = {
        if(id1>id2)line2B :+ int1.get
        else line2B :+ int2.get
      }

      var p1 : Option[Vector2D] = None
      var p2 : Option[Vector2D] = None

      //find out on which part P is. Use the trimmedLine OPPOSITE the trim Point. (the clicked part of the polyline is removed)
      trimGeom match {
        case s : Segment2D => {
          p1 = Some(s.p1)
          p2 = Some(s.p2)
        }
        case _ => println("unsupported trimSegment"+trimGeom)
      }

      //evaluate if the two Vectors of the trimPoint segment exists in the list. If so, the line should not be used.
      val pIsOnLine1 = line1.exists(_._2 == p1.get) && line1.exists(_._2 == p2.get)
      val pIsOnLine2 = line2.exists(_._2 == p1.get) && line2.exists(_._2 == p2.get)

      //TODO: implement trimming of closed PLs when TrimPoint is on same segment as int!
      println(pIsOnLine1)
      println(pIsOnLine2)

      if (pIsOnLine1) trimmedLine =  Some(line2)
      if (pIsOnLine2) trimmedLine = Some(line1)

      //end of if(ints > 1) evaluation
    }
    trimmedLine
  }
}
