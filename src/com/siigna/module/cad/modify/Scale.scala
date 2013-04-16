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
import app.Siigna
import com.siigna.module.cad.create._
import module.ModuleInit

class Scale extends Module {

  var endPoint : Option[Vector2D] = None
  var startPoint : Option[Vector2D] = None
  var middlePoint: Option[Vector2D] = None 
  var firstPointEntered: Boolean = false
  var transformation : TransformationMatrix = TransformationMatrix()
  var requestedLength: Option[Double] = None

  def transformSelection(t : TransformationMatrix) = Drawing.selection.transform(t).shapes.values

  val stateMap: StateMap = Map(

    'Start -> {
      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      case MouseDown(p, MouseButtonRight, _) :: tail => End
      case End(KeyDown(Key.Esc, _)) :: tail => End
      case End(MouseDown(p, MouseButtonRight, _)) :: tail => End

       case End(p : Vector2D) :: tail => {
        if(startPoint.isEmpty){
          startPoint = Some(p)
          //STEP 1: If start point is not set, p is the start point.
          //Next step is to either receive a dragged vector to define the factor to scale with,
          //or a double, which will be the factor,
          //or an other point, which then will be the second reference point.
          //First, the mouse-up is awaited, to see if a drag has occured:
          val vector2DGuide = Vector2DGuide((v: Vector2D) => {
            val scaleFactor = ((v-p).length/100 + 0.25)
            //Define a scaling matrix:
            val t : TransformationMatrix = TransformationMatrix().scale(scaleFactor,p)
            // Return the shape, transformed
            transformSelection(t)
          })
          val inputRequest = InputRequest(Some(vector2DGuide),None,None,None,None,None,None,None,None,Some(9))
          //9: Input type: Coordinates at mouseUp
          //if the base point for scaling is set, goto point with a shape guide
          Start('cad, "create.Input", inputRequest)
        } else if(p != startPoint.get && firstPointEntered == false) {
          //STEP 2a: There is a start, but no endpoint, and a new point is recieved (from mouseUp):
          //If it the coords are the same as mouse up, it is the startpoint, and the mouse has ben clicked, not dragged.
          //If not, it is the end of a drag, defining a scale operation, which is then done:
            val scaleFactor = ((p-startPoint.get).length/100 + 0.25)
            Siigna display ("scale factor: " + scaleFactor)
            transformation = TransformationMatrix().scale(scaleFactor,startPoint.get)
            Drawing.selection.transform(transformation)
            Drawing.deselect()
            End
        } else if (p == startPoint.get && firstPointEntered == false) {
          Siigna display "set second reference point for scaling, type scale factor or drag to scale"
          firstPointEntered = true
          //STEP 2b: The base point for scale has been set. Now either request:
          //1: A scaling factor (Double),
          //2: A point to point out a desired distance to (Vector2D),
          //3: A dragged vector, "dragging" a point to scale the selection 
          val doubleGuide = DoubleGuide((s : Double) => {
            //Define a scaling matrix:
            val t : TransformationMatrix = TransformationMatrix().scale(s,startPoint.get)
            // Return the shape, transformed
            transformSelection(t)
            }
          )
          val inputRequest = InputRequest(None,Some(doubleGuide),None,None,None,None,None,None,None,Some(12))
          Start('cad, "create.Input", inputRequest)
        } else if (firstPointEntered == true && endPoint.isEmpty) {
          //STEP 3: A point is returned (from mouse down). To find out what to do with it, a mouse-up is required:
          endPoint = Some(p)
          val vector2DGuide = Vector2DGuide((v: Vector2D) => {
            val scaleFactor = (startPoint.get.distanceTo(v)/startPoint.get.distanceTo(endPoint.get))
            //Define a scaling matrix:
            val t : TransformationMatrix = TransformationMatrix().scale(scaleFactor,startPoint.get)
            // Return the shape, transformed
            transformSelection(t)
          })
          val inputRequest = InputRequest(Some(vector2DGuide),None,None,None,None,None,None,None,None,Some(9))
          //9: Input type: Coordinates at mouseUp
          Start('cad, "create.Input", inputRequest)
        } else if (firstPointEntered == true && !endPoint.isEmpty && endPoint.get == p) {
          //STEP 4a: A point is returned (from mouse up). If it is the same as the point in step 3, it is
          //the end-point, and the user should:
          // 1) enter the desired distance between the two points to finish the scale, or
          // 2) Leftclick to set scale factor:
          Siigna display "type the distance between the reference points, or move mousa and click to scale"
          val doubleGuide = DoubleGuide((s : Double) => {
            //Define a scaling matrix:
            val t : TransformationMatrix = TransformationMatrix().scale((s/(startPoint.get.distanceTo(endPoint.get))),startPoint.get)
            // Return the shape, transformed
            transformSelection(t)
          })
          val vector2DGuide = Vector2DGuide((v: Vector2D) => {
            val scaleFactor = (startPoint.get.distanceTo(v)/startPoint.get.distanceTo(endPoint.get))
            //Define a scaling matrix:
            val t : TransformationMatrix = TransformationMatrix().scale(scaleFactor,startPoint.get)
            // Return the shape, transformed
            transformSelection(t)
          })
          val inputRequest = InputRequest(Some(vector2DGuide),Some(doubleGuide),None,None,None,None,None,None,None,Some(13))
          Start('cad, "create.Input", inputRequest)
        } else if (firstPointEntered == true && !endPoint.isEmpty && endPoint.get != p) {
          //Step 4b: A drag has occured (the point from mouse up is not the same as from mouse down).
          // or the mouse has been clicked after the end point has been set, defining a scale factor. Do the scaling:
          val scaleFactor = (startPoint.get.distanceTo(p)/startPoint.get.distanceTo(endPoint.get))
          transformation =  TransformationMatrix().scale(scaleFactor,startPoint.get)
          Drawing.selection.transform(transformation)
          Drawing.deselect()
          End
        }
      }
      //if a scaling factor is given:
      case End(l : Double) :: tail => {
        //if a reference length is not set, then scale the shapes by the scale factor.
        if (endPoint.isEmpty) {          
          Siigna display ("scale factor: "+l)
          if (!startPoint.isEmpty) transformation = TransformationMatrix().scale(l,startPoint.get)
          else transformation = TransformationMatrix().scale(l)
          Drawing.selection.transform(transformation)
          Drawing.deselect()
          End
        //if a reference length is set, then point out, what should have this length:
        } else if(!endPoint.isEmpty) {
          //This is the length between start and endpoints after the scale. Do the scale:          
          val scaleFactor = (l/(startPoint.get.distanceTo(endPoint.get)))
          Siigna display ("scale factor:" + scaleFactor)
          transformation = TransformationMatrix().scale(scaleFactor,startPoint.get)
          Drawing.selection.transform(transformation)
          Drawing.deselect()
          End
        }

      }

      case _ => {
        //Should be done differently, but this is how I can reach this (usableSelectionExists) function just quickly...
        val l = new ModuleInit
        if (Drawing.selection.isDefined) {
          Siigna display "set fix-point for scaling, drag to scale or type a scaling factor"
          //Start input, request coordinates from mouse down, or scaling-factor from double,
          //or distance from mouseDown to mouseUp, if a drag occurs:
          Start('cad, "create.Input", 12)
        } else {
          Siigna display "nothing selected"
        End
        }
    } }
  )
}