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

package com.siigna.module.base.create

/* 2012 (C) Copyright by Siigna, all rights reserved. */

import com.siigna._

object Circle extends Module {

  var attributes : Attributes = Attributes()
  def set(name : String, attr : String) = Siigna.get(name).foreach((p : Any) => attributes = attributes + (attr -> p))

  var center : Option[Vector2D] = None
  var radius : Option[Vector2D] = None

  def stateMap = Map(
    //Start: Defines a centerpoint for the circle and forwards to 'SetRadius
    'Start -> {
      case events => {
        events match {
          case MouseDown(_, MouseButtonRight, _) :: tail => 'End
          case Message(p : Vector2D) :: tail => 'SetRadius
          case _ => {
            Module('Point)
          }
        }
        None
      }
    },

    //SetRadius: Listens for the radius of the circle and forwards to 'End
    'SetRadius -> {
      case events => {

        val getCircleGuide : Vector2D => CircleShape = (v : Vector2D) => {
          if (center.isDefined) {
            set("activeLineWeight", "StrokeWidth")
            set("activeColor", "Color")
            CircleShape(center.get, v).setAttributes(attributes)
          } else CircleShape(Vector2D(0,0), Vector2D(0,0))
        }

        events match {
          case Message(p : Vector2D) :: tail => {
            if(center.isDefined) {
              radius = Some(p)
              'End
            } else if (!center.isDefined) {
              center = Some(p)
              //Controller ! Message(PointGuide(getCircleGuide))
              Module('Point)
            }
          }
          case _ =>
        }
      }
    },

    'End -> {
      case _ =>
        //if(Siigna.double("activeLineWeight").isDefined && center.isDefined && radius.isDefined) {
        //  Create(CircleShape(center.get,radius.get).setAttribute("StrokeWidth" -> Siigna.double("activeLineWeight").get))
        //}
        //else Create(CircleShape(center.get,radius.get))

        //create the circle
        set("activeLineWeight", "StrokeWidth")
        set("activeColor", "Color")

        if(center.isDefined && radius.isDefined) Create(CircleShape(center.get,radius.get).setAttributes(attributes))

        //clear the points list
        center = None
        radius = None
    }
  )

}