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
import app.model.shape.PolylineShape.{PolylineShapeClosed, PolylineShapeOpen}
import app.model.shape.{PolylineLineShape, PolylineShape, RectangleShape}
import module.cad.create.InputRequestNew
import util.collection.Attributes

class Trim extends Module {

  private var attr = Attributes()

  val stateMap: StateMap = Map(
    //check if shapes are selected. If not, allow the user to do so.
    'Start -> {
      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      case MouseDown(p, MouseButtonRight, _) :: tail => End
      case End(KeyDown(Key.Esc, _)) :: tail => End
      case End(MouseDown(p, MouseButtonRight, _)) :: tail => End

      //create testshapes
      case KeyDown(Key.ArrowDown, _) :: tail => {
        val lineVert = List(Vector2D(-200,50),Vector2D(-100,0),Vector2D(10,10), Vector2D(100,0))
        val lineHoriz1 = List(Vector2D(30,150),Vector2D(-20,30),Vector2D(-10,20),Vector2D(-15,-30))
        val lineHoriz2 = List(Vector2D(10,20),Vector2D(10,-30))

        Create(PolylineShape(lineVert))
        Create(PolylineShape(lineHoriz1))
        Create(PolylineShape(lineHoriz2))
      }

      //save the selection if returned from the select module
      case End(MouseDown(p, MouseButtonLeft, _)) :: tail => {
        //Dont know what this is / Niels

      }

      case _ => {
        //Go to trim - state only if there is a selection
        println("Underscore")
        if(Drawing.selection.isEmpty) {
          Siigna display "No shapes selected - select shapes to trim"
          Start('cad, "Selection")
        } else {
          println(" selection")
          'Trim
        }
      }
    },

    //when shapes are selected, check for mouse clicks to trim shapes. (TODO: trim by selection box)
    'Trim -> {

      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      case MouseDown(p, MouseButtonRight, _) :: tail => End
      case End(KeyDown(Key.Esc, _)) :: tail => End
      case End(MouseDown(p, MouseButtonRight, _)) :: tail => End


      case End(p : Vector2D) :: tail =>
      case MouseUp(p, _, _) :: tail => {
        val t = View.deviceTransformation
        val point = p.transform(t)
        val nearest = Drawing(point).reduceLeft((a, b) => if (a._2.geometry.distanceTo(point) < b._2.geometry.distanceTo(point)) a else b)
        val trimLine = if (nearest._2.distanceTo(point) < Siigna.selectionDistance) Some(nearest) else None
        //attr = trimLine.get.attributes

        if(trimLine.isDefined) {
          //remove the trimline from the selection to prevent false intersections.
          //TODO: enable trimming of shapes with themselves.
          Drawing.deselect(trimLine.get._1)

          val tl = trimLine.get._2
          tl match {
            //TRIM LINE
            case l : LineShape => {
              println("GOT L: "+l)
              Siigna display ("trimming of lineSegments under construction!")

            }

            //TRIM CLOSED POLYLINES
            case pc : PolylineShapeClosed => {
              val trimmedShapes = TrimmingMethods.trimPolylineClosed(Drawing.selection.shapes,pc,point)

              //if at least one trimmedShapes is defined, delete the original shape:
              if(trimmedShapes.isDefined) {
                Delete(nearest._1)

                //construct new shape
                if(trimmedShapes.isDefined) {
                  Siigna display ("trimming of closed polylines is right around the corner!!")
                  //Create(PolylineShape(trimmedShapes.get.map(_._2)))
                }
              }
            }

            //TRIM OPEN POLYLINES
            case p : PolylineShapeOpen => {
              val trimmedShapes = TrimmingMethods.trimPolylineOpen(Drawing.selection.shapes,p,point)

              //if at least one trimmedShapes is defined, delete the original shape:
              if(trimmedShapes._1.isDefined || trimmedShapes._2.isDefined) {
                Delete(nearest._1)

                //construct new shapes
                if(trimmedShapes._1.isDefined) {
                  //println("t1; "+trimmedShapes._1.get)
                  Create(PolylineShape(trimmedShapes._1.get))
                }
                if(trimmedShapes._2.isDefined) {
                  //println("t2; "+trimmedShapes._2.get)
                  Create(PolylineShape(trimmedShapes._2.get))
                }
              }
            }

            //TRIM RECTANGLES
            case r : RectangleShape => {
              //val rec = PolylineShapeClosed(PolylineLineShape(r.p1,r.p2,r.p3,r))
              val rec = PolylineShapeClosed(r.p0,List(PolylineLineShape(r.p1)),Attributes())
              val trimmedShapes = TrimmingMethods.trimPolylineClosed(Drawing.selection.shapes,rec,point)

              //if at least one trimmedShapes is defined, delete the original shape:
              if(trimmedShapes.isDefined) {
                Delete(nearest._1)

                //construct new shape
                if(trimmedShapes.isDefined) {
                  Siigna display ("trimming of rectangles is right around the corner!!")
                  //Create(PolylineShape(trimmedShapes.get.map(_._2)))
                }
              }
            }

            //UNSUPPORTED SHAPES:
            case e => Siigna display ("sorry, Siigna can not trim " + e.toString.takeWhile(_ != '(') + "s yet!")
          }
        }
        End
      }

      case e => {
        println("error: "+e)
        Siigna display "Click shapes to trim"
        //Requests mouse-down input
        Start('cad,"create.InputNew",InputRequestNew(6,None))
      }
    }
  )
}