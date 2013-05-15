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

import com.siigna._
import com.siigna.app.Siigna
import com.siigna.module.cad.create.InputRequest
import com.siigna.module.ModuleInit
import com.siigna.util.geom.Geometry2D
import java.awt.Color
import module.cad.create.InputRequest
import scala.Some

class Trim extends Module {

  var selection : Option[Selection] = None

  //find the shape closest to the mouse
  def findNearest(m : Vector2D) = {
    var n : Option[(Int, Shape)] = None
    if (Drawing(m).size > 0) {
      val nearest = Drawing(m).reduceLeft((a, b) => if (a._2.geometry.distanceTo(m) < b._2.geometry.distanceTo(m)) a else b)
      n = if (nearest._2.distanceTo(m) < Siigna.selectionDistance) Some(nearest) else None
    }
    n
  }

  val stateMap: StateMap = Map(
    'Start -> {
      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      case MouseDown(p, MouseButtonRight, _) :: tail => End
      case End(KeyDown(Key.Esc, _)) :: tail => End
      case End(MouseDown(p, MouseButtonRight, _)) :: tail => End

      //draw two test shapes
      case KeyDown('a', _) :: tail => {
        val p1 = Vector2D(-20,  0)
        val p2 = Vector2D(10,  20)
        val p3 = Vector2D(20,  10)
        val p4 = Vector2D(0,   40)
        val p5 = Vector2D(0,  -40)
        val p6 = Vector2D(-10,-40)
        val p7 = Vector2D(-10, 40)
        val p8 = Vector2D(-20,-10)
        val p9 = Vector2D(10,  10)
        val p10 = Vector2D(20,  0)
        Create(PolylineShape(p1,p2,p3), LineShape(p4,p5))
        //Create(PolylineShape(p8,p9,p10), LineShape(p6,p7))
        End
      }

      //if a point is returned, evaluate if there is a shape at the given point, and trim by the trim line(s) if possible:
      case End(p : Vector2D) :: tail => {
        Siigna display "click to trim shapes"

        val shapes = selection.get.shapes.toList
        var intersectingIDs = List()
        val nearestShape = findNearest(p)

        if(nearestShape.isDefined) {
          val nearestGeom = nearestShape.get._2.geometry

          println("nearestShape ID: "+nearestShape.get._1)
          shapes.foreach(shape => {
            println("shapeID being evaulated: "+shape._1)
            println("shape Segments being evaulated: "+shape._2.geometry)
            if(shape._2.geometry.intersects(nearestGeom) == true) true
          }) //println("ID OF INTERSECTING SHAPE: "+shape._1))
        } //get the geometry of the nearest shape


        //find shapes which intersect the nearestShape - return the ID of intersection shapes
        //if(s._2.intersect(nearestShape) intersectingIDs += ))
        //trim

      }

      //on first entry, send input request to input to get the start point for the move operations.
      case _ => {
        //if a point (the trimPoint) is returned from input:
        val l = new ModuleInit
        if (l.usableSelectionExists) {
          selection = Drawing.selection //store the selection (to be used as trim guides)
          Siigna display "click to trim shapes"
          val inputRequest = InputRequest(None,None,None,None,None,None,None,None,None,Some(9))
          Start('cad, "create.Input", inputRequest)

        } else {
          Siigna display "please select the shapes to trim"
        }
      }
    }
  )
}