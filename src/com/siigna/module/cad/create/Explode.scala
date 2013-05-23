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

package com.siigna.module.cad.create

/* 2010 (C) Copyright by Siigna, all rights reserved. */


import com.siigna._

/**
 * Created by IntelliJ IDEA.
 * User: oep
 * Date: 09-06-12
 * Time: 11:48
 * To change this template use FileCategory | Settings | FileCategory Templates.
 */

class Explode extends Module{

  var explodedPolylines : Int = 0
  var idsForShapesToExplode: List[Int] = List()
  var polylinesToExplode: List[PolylineShape] = List()

  val stateMap: StateMap = Map(

    'Start -> {
      // GENERAL NOTE: after the Menu forwards to a module with the (presumably) last events,
      // other events may be registered while the ForwardTo mechanism is running.
      // These events can trigger Goto events in the module unless ruled out by case matches.
      
      case _ => {
        //Should be done differently, but this is how I can reach this (usableSelectionExists) function just quickly...
        var somethingExploded: Boolean = false
        if (Drawing.selection.isDefined) {
            println("Drawing.selection.get.self: " + Drawing.selection)
            //Match on shapes in the selection to check for polylines:
            Drawing.selection.foreach((t) => {
              println("Explode begins")
              val id = t._1
              val shape = t._2._1
              val selector = t._2._2
              shape match {
                case p : PolylineShape => {
                  //Check if some of, or the whole shape has been selected:
                  selector match {
                    case FullShapeSelector => {
                      //If the whole shape has been selected, explode it!
                      polylinesToExplode = polylinesToExplode :+ p
                      idsForShapesToExplode = idsForShapesToExplode :+ id
                      somethingExploded = true
                      explodedPolylines += 1
                    }
                    //If only part of the shape has been selected:
                    case BitSetShapeSelector(x) => {
                      println("Bitset x: " + x)
                      x.foreach((bitset) => {
                        //Exclude the endpoints of the polyline:
                        var usefulSplitPoints: Int = 0
                        if (bitset != 0 && bitset != p.size) {
                          usefulSplitPoints = usefulSplitPoints + 1
                          println ("bitset " + bitset )
                          //For now, the splitting only at selected points is not implemented...


                        }
                      })
                    }
                    case _ =>
                  }
                }
                case _ =>
              }
            })
          if (somethingExploded == true) {
            println("Polylines to explode: " + polylinesToExplode)
            polylinesToExplode.foreach((shape) => {
              Create(shape.shapes)
            })
            println("Ids to delete: " + idsForShapesToExplode)
            idsForShapesToExplode.foreach((id) => {
              Delete(id)
            })
            
            Siigna display "polylines in selection exploded"
          } else {
            Siigna display "none of the selected shapes can be exploded"
          }
          End
        } else {
          Siigna display "nothing selected"
          End
        }
      }
    })

}