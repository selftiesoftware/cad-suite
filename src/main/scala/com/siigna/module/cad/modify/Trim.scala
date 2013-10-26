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
import app.model.shape.PolylineLineShape
import app.model.shape.PolylineShape.PolylineShapeClosed
import app.model.shape.PolylineShape.PolylineShapeOpen
import com.siigna.app.model.shape.PolylineShape.{PolylineShapeClosed, PolylineShapeOpen}
import com.siigna.app.model.shape.{PolylineLineShape, PolylineShape, RectangleShape}
import com.siigna.module.cad.create.InputRequest
import com.siigna.module.Tooltip
import com.siigna.util.collection.Attributes
import scala.Some

class Trim extends Module {

  private var attr = Attributes()
  private var selection : Option[Selection] = None

  def m = mousePosition.transform(View.deviceTransformation)

  def nearestShape : Option[(Int, Shape)] = {
    val drawing = Drawing(m)
    if (!drawing.isEmpty) {
      Some(drawing.reduceLeft((a, b) => if (a._2.distanceTo(m) < b._2.distanceTo(m)) a else b))
    } else None
  }

  val stateMap: StateMap = Map(
    //check if shapes are selected. If not, allow the user to do so.
    'Start -> {
      //Exit strategy
      case (End | KeyDown(Key.Esc, _) | End(KeyDown(Key.escape, _)) | MouseDown(_, MouseButtonRight, _) | End(MouseDown(_,MouseButtonRight, _)) ) :: tail => End

      //create testshapes
      case KeyDown(Key.ArrowDown, _) :: tail => {
        val lineVert = List(Vector2D(-200,50),Vector2D(-100,0),Vector2D(10,10), Vector2D(100,0))
        val lineHoriz1 = List(Vector2D(30,150),Vector2D(-20,30),Vector2D(-10,20),Vector2D(-15,-30))
        val lineHoriz2 = List(Vector2D(10,20),Vector2D(10,-30))

        Create(PolylineShape(lineVert))
        Create(PolylineShape(lineHoriz1))
        Create(PolylineShape(lineHoriz2))
      }

      case End(p : Vector2D) :: tail
        if(nearestShape.isDefined) => {
          if (Drawing.selection.isEmpty) {
            //If there isn't a selection, but has been clicked within selection distance of a shape
            // (nearestShape.isDefined), trim nearest shape within selection distance.
            // The shapes that might intersect are those, that are shapes that come closer than the most distant vertex of the
            // shape that should be trimmed. Select those shapes:
            var maxDistance: Double = 0
            nearestShape.get._2.geometry.vertices.foreach(v => {
              if (m.distanceTo(v) > maxDistance) maxDistance = m.distanceTo(v)
            })
            Select(Drawing(m,maxDistance).keys)
            selection = Some(Drawing.selection)
          } else if (selection.isEmpty) {
            selection = Some(Drawing.selection)
          }
          println("Point received")
          val point = p
          val nearest = nearestShape.get
          val trimLine: Option[Shape] = if (nearest._2.distanceTo(point) < Siigna.selectionDistance) Some(nearest._2) else None
          val trimlineId: Option[Int] = if (nearest._2.distanceTo(point) < Siigna.selectionDistance) Some(nearest._1) else None
          println(trimLine)
          if(trimLine.isDefined) {
            println("trimline defined")
            attr = trimLine.get.attributes
            //remove the trimline from the selection to prevent false intersections.
            //TODO: enable trimming of shapes with themselves.
            Drawing.deselect(trimlineId.get)

            val tl = trimLine.get
            tl match {

              //TRIM ARCS
              case a : ArcShape => {
                Siigna display("We are sorry - trimming of arcs not yet implemented.")
                Tooltip.blockUpdate(3500)
              }

              //TRIM LINE
              case l : LineShape => {
                val trimmedShapes = TrimmingMethods.trimLine(Drawing.selection.shapes,l,point)

                //if at least one trimmedShapes is defined, delete the original shape:
                if(trimmedShapes._1.isDefined || trimmedShapes._2.isDefined) {
                  Delete(nearest._1)

                  //construct new shapes
                  if(trimmedShapes._1.isDefined) {
                    val line1 = LineShape(trimmedShapes._1.get).addAttributes(attr)
                    Create(line1)
                    //add the line to the selection
                    val shapeInModel  = Drawing.get(com.siigna.app.Siigna.latestID.get).get
                    selection = Some(selection.get.add(com.siigna.app.Siigna.latestID.get,(shapeInModel,FullShapeSelector)))
                  }
                  if(trimmedShapes._2.isDefined) {
                    val line2 = LineShape(trimmedShapes._2.get).addAttributes(attr)
                    Create(line2)
                    val shapeInModel  = Drawing.get(com.siigna.app.Siigna.latestID.get).get
                    selection = Some(selection.get.add(com.siigna.app.Siigna.latestID.get,(shapeInModel,FullShapeSelector)))
                  }
                }
              }

              //TRIM CLOSED POLYLINES
              case pc : PolylineShapeClosed => {
                val trimmedShapes = TrimmingMethods.trimPolylineClosed(Drawing.selection.shapes,pc,point)

                //if at least one trimmedShapes is defined, delete the original shape:
                if(trimmedShapes.isDefined) {
                  Delete(nearest._1)
                  //construct new shape

                  val line = PolylineShape(trimmedShapes.get.map(_._2)).addAttributes(attr)
                  Create(line)
                  val shapeInModel  = Drawing.get(com.siigna.app.Siigna.latestID.get).get
                  selection = Some(selection.get.add(com.siigna.app.Siigna.latestID.get,(shapeInModel,FullShapeSelector)))
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
                    val line1 = PolylineShape(trimmedShapes._1.get).addAttributes(attr)
                    Create(line1)
                    val shapeInModel  = Drawing.get(com.siigna.app.Siigna.latestID.get).get
                    selection = Some(selection.get.add(com.siigna.app.Siigna.latestID.get,(shapeInModel,FullShapeSelector)))
                  }
                  if(trimmedShapes._2.isDefined) {
                    val line2 = PolylineShape(trimmedShapes._2.get).addAttributes(attr)
                    Create(line2)
                    val shapeInModel  = Drawing.get(com.siigna.app.Siigna.latestID.get).get
                    selection = Some(selection.get.add(com.siigna.app.Siigna.latestID.get,(shapeInModel,FullShapeSelector)))
                  }
                }
              }

              //TRIM RECTANGLES
              case r : RectangleShape => {
                //val rec = PolylineShapeClosed(PolylineLineShape(r.p1,r.p2,r.p3,r))

                //convert the rectangle to a closed Polyline:
                val rec = PolylineShapeClosed(r.p0,List(PolylineLineShape(r.p1),PolylineLineShape(r.p2),PolylineLineShape(r.p3)),Attributes())
                val trimmedShapes = TrimmingMethods.trimPolylineClosed(Drawing.selection.shapes,rec,point)

                //if at least one trimmedShapes is defined, delete the original shape:
                if(trimmedShapes.isDefined) {
                  Delete(nearest._1)

                  //construct new shape
                  if(trimmedShapes.isDefined) {
                    //Siigna display ("trimming of rectangles is right around the corner!!")
                    Create(PolylineShape(trimmedShapes.get.map(_._2)).addAttributes(attr))
                    val shapeInModel  = Drawing.get(com.siigna.app.Siigna.latestID.get).get
                    selection = Some(selection.get.add(com.siigna.app.Siigna.latestID.get,(shapeInModel,FullShapeSelector)))
                  }
                }
              }

              //UNSUPPORTED SHAPES:
              case e => Siigna display ("sorry, Siigna can not trim " + e.toString.takeWhile(_ != '(') + "s yet!")
              Tooltip.blockUpdate(3500)
            }

            //remove the original trimline from the selection
            selection = Some(Selection(selection.get - trimlineId.get))
            //and select the new selection
            Drawing.select(selection.get)
          }

        }
      case e => {
        Siigna display "Click shapes to trim"

        Tooltip.blockUpdate(3500)
        //Requests mouse-down input
        Start('cad,"create.Input", InputRequest(2,None))
      }
    }
  )
}