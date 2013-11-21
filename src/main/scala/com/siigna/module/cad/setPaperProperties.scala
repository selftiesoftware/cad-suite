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

import com.siigna.app.Siigna
import com.siigna.app.model.Drawing
import com.siigna.util.geom.{Rectangle2D, TransformationMatrix, Vector2D}
import com.siigna.app.view.{Graphics, View}
import com.siigna.module.{Module, Tooltip}
import com.siigna.app.model.shape.{Shape, TextShape, PolylineShape, LineShape}
import com.siigna.util.event.Start
import com.siigna.module.cad.helpers._
import com.siigna._
import scala.Some
import com.siigna.module.cad.create.{InputRequest, DynamicDrawFromText}

/*
paper properties changeable by the user
 */


object setPaperProperties{
  /**
   *
   * @param p the location of the mouse
   * @param click true if click, false if mouse move
   * @return Boolean: true if a click was registered,
   *         Option [Shape] the shape to draw,
   *         Boolean: true if SetPaperScale module should be called.
   */

  def paperChangeCheck(p: Vector2D, click : Boolean) : (Boolean, Option[Shape], Boolean) = {
    //paper header interaction check (for setting paper scale and size)
    val b = Drawing.boundaryScale
    val br = Drawing.boundary.bottomRight
    var returnShape : Option[Shape] = None
    var gotoScale = false

    //a shape to illustrate that a clickable area is active
    def sUp(v : Vector2D) = PolylineShape(Vector2D(-1.6,-1)+v,Vector2D(0,1.2)+v,Vector2D(1.6,-1)+v,Vector2D(-1.6,-1)+v)
    def sDown(v : Vector2D) = PolylineShape(Vector2D(-1.6,1)+v,Vector2D(0,-1.2)+v,Vector2D(1.6,1)+v,Vector2D(-1.6,1)+v)

    //feedback to paper scale changing
    def autoButton(v : Vector2D) = TextShape("A", v,3)

    //feedback to typing paper scale
    def setScaleFrame(v : Vector2D) = PolylineShape(Rectangle2D(v + Vector2D(-13,3),v + Vector2D(13,-3)))

    var r = false // a boolean telling if the click was inside an active area

    if(((br + Vector2D(-2.5*b,5*b)) - p.transform(View.deviceTransformation)).length < 1.5*b) {
      Tooltip.updateTooltip(List("Double click to set the paper scale automatically"))
      if(click) setAutoScale()
      returnShape = Some(autoButton(br + Vector2D(-3*b,7*b)))
      r = true
    }
    //initiate the SetPaperScale module
    else if(((br + Vector2D(-18*b,4*b)) - p.transform(View.deviceTransformation)).length < 7*b) {
      Tooltip.updateTooltip(List("Double click to type a paper scale"))
      if(click) {
        //Start('cad, "SetPaperScale", inputRequest)
        gotoScale = true
      }
      returnShape = Some(setScaleFrame(br +Vector2D(-18*b,4*b)))
      r = true
    }

    else if(((br + Vector2D(-42.5*b,5*b)) - p.transform(View.deviceTransformation)).length < 1.5*b)  {
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
      gotoScale = false
    }
    (r,returnShape,gotoScale)
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
    //TODO: OUCH not good - a bad hack to update the model. How to otherwise register boundary changes??
    Drawing.undo()
    Drawing.redo()

  }
  //toggle automatic and manual paper scale setting
  def setAutoScale() = {
    println("set paper auto scale mode")

  }
}