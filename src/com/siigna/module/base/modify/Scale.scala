/*
 * Copyright (c) 2012. Siigna is released under the creative common license by-nc-sa. You are free
 * to Share — to copy, distribute and transmit the work,
 * to Remix — to adapt the work
 *
 * Under the following conditions:
 * Attribution —  You must attribute the work to http://siigna.com in the manner specified by the author or licensor (but not in any way that suggests that they endorse you or your use of the work).
 * Noncommercial — You may not use this work for commercial purposes.
 * Share Alike — If you alter, transform, or build upon this work, you may distribute the resulting work only under the same or similar license to this one.
 */

package com.siigna.module.base.modify

import com.siigna._
import app.Siigna
import com.siigna.module.base.create._

class Scale extends Module {

  var endPoint : Option[Vector2D] = None
  var startPoint : Option[Vector2D] = None
  var middlePoint: Option[Vector2D] = None 
  var firstPointEntered: Boolean = false
  var transformation : Option[TransformationMatrix] = None
  var requestedLength: Option[Double] = None

  val stateMap: StateMap = Map(

    'Start -> {
       case End(p : Vector2D) :: tail => {
        if(startPoint.isEmpty){
          startPoint = Some(p)
          //If start point is not set, p is the reference point.
          //Next step is to either receive a dragged vector to define the factor to scale with,
          //or a double, which will be the factor,
          //or an other point, which then will be the basis for defining the factor
          val shapeGuide = PointGuide((v : Vector2D) => {
            val scaleFactor = ((v-p).length/100 + 0.25)
            //Define a scaling matrix:
            val t : TransformationMatrix = TransformationMatrix().scale(scaleFactor,p)
            // Return the shape, transformed
            Drawing.selection.get.apply(t)
          },9) //9: Input type: Coordinates at mouseUp
          //if the base point for scaling is set, goto point with a shape guide
          Start('Input,"com.siigna.module.base.create", shapeGuide)

        } else if(p != startPoint.get && firstPointEntered == false) {
          //If there is a start, but no endpoint, and a new point is recieved (from mouseUp),
          //If it the coords are the same as mouse up, it is the startpoint,
          // if not it is the end of a drag, defining a scale operation, which is then done:
            val scaleFactor = ((p-startPoint.get).length/100 + 0.25)
            Siigna display ("scale factor: " + scaleFactor)
            transformation =  Some(TransformationMatrix().scale(scaleFactor,startPoint.get))
            Drawing.selection.get.transform(transformation.get)
            Drawing.deselect()
            End
          } else if (p == startPoint.get && firstPointEntered == false) {
            Siigna display "set reference point for scaling, or type scale factor"
            firstPointEntered = true
            //The base point for scale has been set. Now either:
            //1: A scaling factor (Double),
            //2: A point to "grab" for the scaling (Vector2D) is needed.
            val shapeGuide = DoubleGuide((s : Double) => {
              //Define a scaling matrix:
              val t : TransformationMatrix = TransformationMatrix().scale(s)
              // Return the shape, transformed
              Drawing.selection.get.apply(t)
            },1031) //1031: Input type: Double by keyboard, or Vector2D by leftclick.
            Start('Input,"com.siigna.module.base.create", shapeGuide)
          } else if (firstPointEntered == true && middlePoint.isEmpty) {
            Siigna display "set endpoint for scaling, or type desired length to point out"
            middlePoint = Some(p)
            //If a second point is received, the next can be:
            //1: A double, to define the desired new length of something, or
            //2: A point, where the user wants to "leave" the last point
            val shapeGuide = PointGuide((v : Vector2D) => {
              val scaleFactor = (((v - startPoint.get).length)/(middlePoint.get - startPoint.get).length)
              //Define a scaling matrix:
              val t : TransformationMatrix = TransformationMatrix().scale(scaleFactor,startPoint.get)
              // Return the shape, transformed
              Drawing.selection.get.apply(t)
            },1031) //1031: Input type: Double by keyboard, or Vector2D by leftclick.
            //if the base point for scaling is set, goto point with a shape guide
            Start('Input,"com.siigna.module.base.create", shapeGuide)
          } else if (!middlePoint.isEmpty && requestedLength.isEmpty) {
            //If a third point is recieved, and there is not any requested length set, the scaling is finished:
            val scaleFactor = (((p - startPoint.get).length)/(middlePoint.get - startPoint.get).length)
            Siigna display ("scale factor: " + scaleFactor)
            transformation =  Some(TransformationMatrix().scale(scaleFactor,startPoint.get))
            Drawing.selection.get.transform(transformation.get)
            Drawing.deselect()
            End
          } else if (!middlePoint.isEmpty && !requestedLength.isEmpty) {
            //Then the third click defines the point,
            //that has to have the requested distance from the middle point after the scaling:
            val scaleFactor = (requestedLength.get/(p - middlePoint.get).length)
            println("Scale factor: " + scaleFactor)
            transformation =  Some(TransformationMatrix().scale(scaleFactor,startPoint.get))
            Drawing.selection.get.transform(transformation.get)
            Drawing.deselect()
            End
          }

          /*endPoint = Some(p)
          Siigna display "set scaling factor"

          val shapeGuide = PointPointGuide(p,(v : Vector2D) => {
            val refScale : Vector2D = startPoint.get - endPoint.get
            val scaleFactor = ((v - startPoint.get).length/refScale.length)
            //Define a scaling matrix:
              val t : TransformationMatrix = TransformationMatrix(Vector2D(0,0), 1).scale(scaleFactor,startPoint.get)
            // Return the shape, transformed
            Drawing.selection.get.apply(t)
          },2)//2 : Input type = InputOneValue

          //if the base point for scaling is set, goto point with a shape guide
          Start('Point,"com.siigna.module.base.create", shapeGuide)
        }
        else if(startPoint.isDefined && endPoint.isDefined){
          val refScale : Vector2D = startPoint.get - endPoint.get
          val scaleFactor = ((p - startPoint.get).length/refScale.length)

          Siigna display ("scale factor: "+scaleFactor)

          transformation =  Some(TransformationMatrix().scale(scaleFactor,startPoint.get))
          Drawing.selection.get.transform(transformation.get)
          Drawing.deselect()
          End
        }  */
      }
      //if a scaling factor is given:
      case End(l : Double) :: tail => {
        //if a reference length is not set, then scale the shapes by the scale factor.
        if (!middlePoint.isDefined) {
          Siigna display ("scale factor: "+l)
          transformation =  Some(TransformationMatrix().scale(l,startPoint.get))
          Drawing.selection.get.transform(transformation.get)
          Drawing.deselect()
          End
        //if a reference length is set, then point out, what should have this length:
        } else if(middlePoint.isDefined) {
          //We need is a point from the drawing, defined by leftclick (input type 11):
          //A lineGuide helps...
          requestedLength = Some(l)
          Siigna display ("click on the point, that you want to be " + l + " away")
          val guide = PointGuide((v : Vector2D) => {
            (Array(LineShape(middlePoint.get, v)))
          },11)//1 : Input type = InputTwoValues
          Start('Input,"com.siigna.module.base.create", guide)
        }

      }
      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      case MouseDown(p, MouseButtonRight, _) :: tail => End

      case _ => {
        Siigna display "set base point for scaling, or drag to scale"
        //Start input, request coordinates from mouse down, or key-input,
        //or distance from mouseDown to mouseUp, if a drag occurs:
        Start('Input,"com.siigna.module.base.create",1)
      }
    }
  )
}