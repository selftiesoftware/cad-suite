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


package com.siigna.module.cad

import com.siigna.app.model.shape.{PolylineShape, LineShape, Shape}
import com.siigna.util.collection.Attributes
import com.siigna.app.model.shape.PolylineShape.PolylineShapeOpen
import com.siigna.util.event.End
import com.siigna.util.geom.Vector2D


/*
Methods used to join lines or polylines into polylines
 */

object joinMethods {

  //a function to round a vector2D to a tolerance
  def epsilon (v : Vector2D) : Vector2D = Vector2D(math.round(v.x * 100000)/100000.toDouble,math.round(v.y * 100000)/100000.toDouble)

  /**
   * check if two shapes have coinsiding ends
   * @param s1 first Shape to evaluate
   * @param s2 second Shape to evaluate
   * @return true if ends coinside, false if they do not.
   */
  def endsCheck(s1 : Shape, s2 : Shape) : Boolean = {
    val start1 = epsilon(s1.geometry.vertices.head)
    val end1 = epsilon(s1.geometry.vertices.last)
    val start2 = epsilon(s2.geometry.vertices.head)
    val end2 = epsilon(s2.geometry.vertices.last)

    //check for coinsiding ends and join shapes if such are found
    if(start1 == start2 || start1 == end2 || end1 == start2 || end2 == start2 || end1 == end2) {
      true
    } else {
      false
    }
  }

  /**
   * join two lists of vertices if their ends coinside
   * @param l1 first list of vectors to evaluate
   * @param l2 second list of vectors to evaluate
   * @return  None if two shapes if an end do not coinside. A new list vectors for a joined line if they do.
   */

  def addTwoVerticeLists(l1 : List[Vector2D], l2 : List[Vector2D]) : List[Vector2D] = {
    val s1 = epsilon(l1.head)
    val e1 = epsilon(l1.last)
    val s2 = epsilon(l2.head)
    val e2 = epsilon(l2.last)

    //catch lists which will result in closed polylines
    if((s1 == e2 && s2 == e1) || (s1 == s2 && e2 == e1)) {
      if(e1 == s2) l1 ++ l2 else l1.reverse ++ l2
    }
    //catch lists which will result in open polylines
    else {
      //align list1
      val newL1 = if(s1 == s2) {
        l1.reverse
      } else l1

      //align list2
      val newL2 = if(e1 == e2) {
        l2.reverse
      } else l2

      newL1 ++ newL2
      if(s1 == e2) newL1.reverse ++ newL2.reverse else newL1 ++ newL2 //prevent false construction of closed PLs
    }
  }

  /**
   * Checks if two shapes are either lines or polylines,
   * and calls the 'endsCheck' and 'addTwoVerticeLists' functions to see if they should be joined.
   * @param s1 first Shape to evaluate
   * @param s2 second Shape to evaluate
   * @param attr attributes of the first shape, will be given to the joined shape.
   * @return Some[Shape] if successful, None if shapes cannot be joined.
   */
  def joinTwoShapes(s1: Shape, s2: Shape, attr: Attributes) : Option[Shape] = {

    var s1vertices : List[Vector2D] = List()
    var s2vertices : List[Vector2D] = List()

    //check that the shapes are line or polyline shapes
    if(s1.isInstanceOf[LineShape] || s1.isInstanceOf[PolylineShapeOpen]) {
      if(s2.isInstanceOf[LineShape] || s2.isInstanceOf[PolylineShapeOpen]) {
        //if ends coinside, join the shapes
        if(endsCheck(s1,s2)) {
          //get the vertices of line one
          s1 match {
            case l : LineShape => s1vertices = List(l.p1,l.p2)
            case p : PolylineShapeOpen => s1vertices = p.geometry.vertices.toList
            case _ => End
          }

          //get the vertices of line two
          s2 match {
            case l : LineShape => s2vertices = List(l.p1,l.p2)
            case p : PolylineShapeOpen => s2vertices = p.geometry.vertices.toList
            case _ => End
          }
          //join (poly)lines
          val s = addTwoVerticeLists(s1vertices,s2vertices)
          if(!s.isEmpty) Some(PolylineShape(s).addAttributes(attr)) else None //return the joined shape if any.
        } else None
      } else None
    } else None
  }

  def joinMultiple(sMap : Map[Int,Shape]) : List[Shape] = {

    //make a var with shapes in sMap
    var shapes = sMap.map(s => s._2).toList
    var returnList : List[Shape] = List()
    //for all the shapes in shapes:
    for(i <- 1 to sMap.size) {
      //try if the first shape in shapes can be joined with any one of the following shapes.
      val evalShape = shapes.head
      val rest = shapes.tail
      //clear the shapes var
      shapes = List()
      //go through all the remaining shapes and see if the first shape joins with any:
      rest.foreach(s => {
        val res = joinTwoShapes(evalShape,s,s.attributes)
        if(res != None){
          if(i == sMap.size - 1) {
            returnList = returnList :+ res.get
            shapes = shapes :+ res.get
          } else {
          //println("joining: "+evalShape +" + "+s)
          //if so, replace the shape with the joined shape
          //println("res; "+ shapes :+ res.get)
          shapes = shapes :+ res.get //add the joined shape to the shapes list for use in next evaluation

          //evalShape does not join with s, then return s only.
          // TODO: if evalShape does not join with any shapes, it is not returned?!
          }
        } else {
          //if last iteration, return everything
          if(i == sMap.size - 1) {
            shapes = shapes :+ s
            returnList = returnList :+ evalShape
            returnList = returnList :+ s
          }
          //if not, save the shape for later evaulation.
          else {
            shapes = shapes :+ s
          } //put s back in shapes
        }
      })
    }
    //return returnList
    returnList
  }

}
