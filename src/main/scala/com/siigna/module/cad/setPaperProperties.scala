package com.siigna.module.cad

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

import com.siigna.app.Siigna
import com.siigna.app.model.Drawing
<<<<<<< HEAD
import com.siigna.app.view.View
=======
import com.siigna.util.geom.{TransformationMatrix, Vector2D}
import com.siigna.app.view.{Graphics, View}
import com.siigna.module.{Module, Tooltip}
import com.siigna.app.model.shape.{PolylineShape, LineShape}
>>>>>>> 01ecccac660f2a88a96fb066c11f09d99d6fbdc5

/*
paper properties changeable by the user
 */

//change paper properties
object setPaperProperties{
  def paperChangeCheck(p: Vector2D, click : Boolean) : (Boolean, Option[PolylineShape]) = {
    //paper header interaction check (for setting paper scale and size)
    val b = Drawing.boundaryScale
    val br = Drawing.boundary.bottomRight
    var returnShape : Option[PolylineShape] = None
    //a shape to illustrate that a clickable area is active
    def sUp(v : Vector2D) = PolylineShape(Vector2D(-1.6,-1)+v,Vector2D(0,1.2)+v,Vector2D(1.6,-1)+v,Vector2D(-1.6,-1)+v)
    def sDown(v : Vector2D) = PolylineShape(Vector2D(-1.6,1)+v,Vector2D(0,-1.2)+v,Vector2D(1.6,1)+v,Vector2D(-1.6,1)+v)

    var r = false
    /*
    if(((br + Vector2D(-2.5*b,5*b)) - p.transform(View.deviceTransformation)).length < 1.5*b) {
      Tooltip.updateTooltip(List("Double click to increase the paper scale"))
      if(click) changeScale(true)
      returnShape = Some(sUp(br + Vector2D(-2.5*b,5*b)))
      r = true
    }

    else if(((br + Vector2D(-2.5*b,2*b)) - p.transform(View.deviceTransformation)).length < 1.5*b) {
      Tooltip.updateTooltip(List("Double click to decrease the paper scale"))
      if(click) changeScale(false)
      returnShape = Some(sDown(br +Vector2D(-2.5*b,2*b)))
      r = true
    }
    */
    if(((br + Vector2D(-42.5*b,5*b)) - p.transform(View.deviceTransformation)).length < 1.5*b)  {
      Tooltip.updateTooltip(List("Double click to increase the paper size"))
      if(click)changeSize(true)
      returnShape = Some(sUp(br +Vector2D(-42.5*b,5*b)))
      r = true
    }
    else if(((br + Vector2D(-42.5*b,2*b)) - p.transform(View.deviceTransformation)).length < 1*b) {
      Tooltip.updateTooltip(List("Double click to decrease the paper size"))
      if(click) changeSize(false)
      returnShape = Some(sDown(br +Vector2D(-42.5*b,2*b)))
      r = true
    }
    else {
      if(!r) Tooltip.updateTooltip(List()) //clear tooltip display
      r = false
      returnShape = None
    }
    (r,returnShape)
  }

  //change paper size
  def changeSize(increase : Boolean) = {
    val step = math.sqrt(2.0) //step from one A-size to the next. Equals 1.41421356
    val min = Siigna.double("printFormatMin").get
    val max = Siigna.double("printFormatMax").get
    val belowMinSize : Boolean = Siigna.double("printFormatMax").get < 298
    val aboveMaxSize : Boolean = Siigna.double("printFormatMax").get > 1187

    if(increase && !aboveMaxSize) {
      Siigna("printFormatMin") = min * step
      Siigna("printFormatMax") = max * step
    }
    else if(!increase && !belowMinSize){
      Siigna("printFormatMin") = min / step
      Siigna("printFormatMax") = max / step
    }
    //recalulate the boundary
    Drawing.calculateBoundary()

    View.zoomExtends
    //TODO: OUCH not good - a bad hack to update the model. How to otherwise register boundary changes??
    Drawing.undo()
    Drawing.redo()

  }
  //change paper scale
  def changeScale(increase : Boolean) = {
    if(increase) println("increase")
    else println("decrease")
  }
}
