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

package com.siigna.module.cad.properties

import com.siigna._
import com.siigna.app.model.selection

/**
 * A module to transfer properties from one shape to a selection of shapes.
 */
class SampleProperties extends Module{

  var attributes : Attributes = Attributes()
  //var color : Option[String] = None
  var selected : Option[selection.Selection] = None
  var templateShape : Option[Shape] = None
  //var weight : Option[String] = None

  def stateMap : StateMap = Map(
    'Start -> {
      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      case MouseDown(p, MouseButtonRight, _) :: tail => End
      case End(KeyDown(Key.Esc, _)) :: tail => End
      case End(MouseDown(p, MouseButtonRight, _)) :: tail => End

      case _ => {
        Siigna display ("select an object to sample from")

        if (Drawing.selection.isDefined) {
          templateShape = Some(Drawing.selection.shapes.head._2)
          attributes = templateShape.get.attributes
          Drawing.deselect()
          'UpdateShapes
        } else Module('cad, "base.Selection")

      }
    },
    'UpdateShapes -> {
      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      case MouseDown(p, MouseButtonRight, _) :: tail => End
      case End(KeyDown(Key.Esc, _)) :: tail => End
      case End(MouseDown(p, MouseButtonRight, _)) :: tail => End

      case _ => {
        Siigna display ("select objects to update")
        if(Drawing.selection.isDefined) 'End
        else Module('cad, "base.Selection")
      }
    },
    'End -> {
      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      case MouseDown(p, MouseButtonRight, _) :: tail => End
      case End(KeyDown(Key.Esc, _)) :: tail => End
      case End(MouseDown(p, MouseButtonRight, _)) :: tail => End

      case _ => {
        if(Drawing.selection.isDefined) {
          Drawing.selection.setAttributes(attributes)
          Drawing.deselect()
        }
        //clear vars
        attributes = Attributes()
        selected = None
        templateShape = None
      }
    }
  )
}