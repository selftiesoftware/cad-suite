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
  var transformation : Option[TransformationMatrix] = None

  val stateMap: StateMap = Map(

    'Start -> {
      case End(p : Vector2D) :: tail => {
        if(!startPoint.isDefined){
          startPoint = Some(p)

          val shapeGuide = PointGuide(p,(v : Vector2D) => {
            val refScale : Vector2D = startPoint.get - p
            val scaleFactor = ((v - startPoint.get).length/refScale.length)
            //Define a scaling matrix:
            val t : TransformationMatrix = TransformationMatrix(Vector2D(0,0), 1).scale(scaleFactor,startPoint.get)
            // Return the shape, transformed
            Drawing.selection.get.apply(t)
          },2)//2 : Input type = InputOneValue

          Siigna display "type scale factor or a set reference point for scaling"
          Start('Point,"com.siigna.module.base.create", shapeGuide)
        }

        else if(startPoint.isDefined && !endPoint.isDefined) {
          endPoint = Some(p)
          Siigna display "set scaling factor"

          val shapeGuide = PointGuide(p,(v : Vector2D) => {
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
        }
      }
      //if a scaling factor is given:
      case End(l : Double) :: tail => {
        //if a reference length is not set, then scale the shapes by the scale factor.
        if (!endPoint.isDefined) {

          Siigna display ("scale factor: "+l)

          transformation =  Some(TransformationMatrix().scale(l,startPoint.get))
          Drawing.selection.get.transform(transformation.get)
          Drawing.deselect()
          End


        //if a reference length is set, then scale this length to match the length typed.
        } else if(endPoint.isDefined) {

          val refScale : Vector2D = startPoint.get - endPoint.get
          var scaleFactor = (l/refScale.length)

          Siigna display ("reference length: "+l)

          transformation = Some(TransformationMatrix(Vector2D(0,0),1))
          Drawing.selection.get.transform(transformation.get.scale(scaleFactor,startPoint.get))
          Drawing.deselect()
          End
        }

      }
      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      case MouseDown(p, MouseButtonRight, _) :: tail => End

      case _ => {
        Siigna display "set base point for scaling"
        Start('Point,"com.siigna.module.base.create")
      }
    }
  )
}