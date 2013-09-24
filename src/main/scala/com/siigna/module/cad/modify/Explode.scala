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

/* 2010 (C) Copyright by Siigna, all rights reserved. */


import com.siigna._
import app.model.shape.PolylineShape.{PolylineShapeOpen, PolylineShapeClosed}
import app.model.shape.{InnerPolylineShape, RectangleShape}
import java.util

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
            //Match on shapes in the selection to check for polylines:
            Drawing.selection.foreach((t) => {
              println("Explode begins")
              val id = t._1
              val shape = t._2._1
              val selector = t._2._2
              shape match {
                case p : PolylineShapeOpen => {
                  //Check if some of, or the whole shape has been selected:
                  selector match {
                    case FullShapeSelector => {
                      //If the whole shape has been selected, explode it!
                      idsForShapesToExplode = idsForShapesToExplode :+ id
                      somethingExploded = true
                      p.shapes.foreach((shape) => {
                        Create(shape.addAttributes(p.attributes))
                      })
                    }
                    //If only part of the shape has been selected:
                    case BitSetShapeSelector(x) => {
                      var exploded: Boolean = false
                      var lastBitSet: Int = 0
                      var leftover: Seq[InnerPolylineShape] = p.innerShapes
                      x.foreach((bitset) => {
                        //Exclude the endpoints of the polyline:
                        if (bitset != 0 && bitset != p.size && p.size > 1) {
                          exploded = true
                          if (bitset == 1) {
                            Create(LineShape(p.startPoint,p.innerShapes(bitset-1).point).addAttributes(p.attributes))
                            leftover = p.innerShapes.splitAt(bitset-1)._2
                            lastBitSet = bitset
                          } else {
                            if (bitset - lastBitSet == 1) { //The neighbouring point is selected - make a line and a leftover...
                              Create(LineShape(p.innerShapes(bitset-2).point,p.innerShapes(bitset-1).point).addAttributes(p.attributes))
                              leftover = p.innerShapes.splitAt(bitset-1)._2
                              lastBitSet = bitset
                            } else { //The neighbour isn't selected - make a polyline and if there is enough left, a leftover, else a line...
                              var firstPart: Seq[Vector2D] = Seq()
                              if(leftover.length > 2) { //If there's enough left, make a new leftower,
                                if (lastBitSet == 0) {
                                  firstPart = firstPart :+ p.startPoint
                                  leftover.splitAt(bitset - lastBitSet)._1.foreach(innerShape => firstPart = firstPart :+ innerShape.point)
                                } else leftover.splitAt((bitset + 1) - lastBitSet)._1.foreach(innerShape => firstPart = firstPart :+ innerShape.point)
                                leftover = p.innerShapes.splitAt(bitset-1)._2
                                lastBitSet = bitset
                                Create(PolylineShape(firstPart).addAttributes(p.attributes))
                              } else { //Otherwise make it into a line...
                                Create(LineShape(p.innerShapes(bitset-1).point, p.innerShapes(bitset).point).addAttributes(p.attributes))
                              }
                            }
                          }
                        }
                      })
                      if (exploded == true) {
                        var firstPart: Seq[Vector2D] = Seq()
                        if(leftover.length > 2) { //If there's enough left, make a new leftower,
                          leftover.foreach(innerShape => firstPart = firstPart :+ innerShape.point)
                          Create(PolylineShape(firstPart).addAttributes(p.attributes))
                        } else if (leftover.length == 2) { //Otherwise make it into a line...
                          Create(LineShape(p.innerShapes(p.innerShapes.length - 2).point, p.innerShapes(p.innerShapes.length-1).point).addAttributes(p.attributes))
                        }
                        idsForShapesToExplode = idsForShapesToExplode :+ id
                        somethingExploded = true
                      }
                    }
                    case _ =>
                  }
                }
                case p : RectangleShape => {
                  selector match {
                    case FullShapeSelector => {
                      Create(LineShape(p.p0,p.p1).addAttributes(p.attributes))
                      Create(LineShape(p.p1,p.p2).addAttributes(p.attributes))
                      Create(LineShape(p.p2,p.p3).addAttributes(p.attributes))
                      Create(LineShape(p.p3,p.p0).addAttributes(p.attributes))
                      idsForShapesToExplode = idsForShapesToExplode :+ id
                      somethingExploded = true
                    }
                    case BitSetShapeSelector(x) => {
                      var firstBitSet: Option[Int] = None
                      if (x.size == 1) {
                        if (x(0) == true) Create(PolylineShape.createOpen(p.p0,p.p1,p.p2,p.p3,p.p0).addAttributes(p.attributes))
                        if (x(1) == true) Create(PolylineShape.createOpen(p.p1,p.p2,p.p3,p.p0,p.p1).addAttributes(p.attributes))
                        if (x(2) == true) Create(PolylineShape.createOpen(p.p2,p.p3,p.p0,p.p1,p.p2).addAttributes(p.attributes))
                        if (x(3) == true) Create(PolylineShape.createOpen(p.p3,p.p0,p.p1,p.p2,p.p3).addAttributes(p.attributes))
                        idsForShapesToExplode = idsForShapesToExplode :+ id
                      } else if (x.size == 2) {
                        x.foreach(bitSet => {
                          if (firstBitSet.isEmpty) firstBitSet = Some(bitSet)
                          if (bitSet != firstBitSet.get){
                            //Next to each other: A line and a polyline.
                            if (bitSet - firstBitSet.get == 1 || bitSet - firstBitSet.get == 3 ) {
                              if (firstBitSet.get == 0) {
                                Create(LineShape(p.p3,p.p0).addAttributes(p.attributes))
                                Create(PolylineShape(p.p0,p.p1,p.p2,p.p3).addAttributes(p.attributes))
                              } else if (firstBitSet.get == 1) {
                                Create(LineShape(p.p0,p.p1).addAttributes(p.attributes))
                                Create(PolylineShape(p.p1,p.p2,p.p3,p.p0).addAttributes(p.attributes))
                              } else if (firstBitSet.get == 2) {
                                Create(LineShape(p.p1,p.p2).addAttributes(p.attributes))
                                Create(PolylineShape(p.p2,p.p3,p.p0,p.p1).addAttributes(p.attributes))
                              } else if (firstBitSet.get == 3) {
                                Create(LineShape(p.p2,p.p3).addAttributes(p.attributes))
                                Create(PolylineShape(p.p3,p.p0,p.p1,p.p2).addAttributes(p.attributes))
                              }
                            } else { //Not next to each others: Two polylines.
                              if (firstBitSet.get == 0) {
                                Create(PolylineShape(p.p0,p.p1,p.p2).addAttributes(p.attributes))
                                Create(PolylineShape(p.p2,p.p3,p.p0).addAttributes(p.attributes))
                              } else if (firstBitSet.get == 1) {
                                Create(PolylineShape(p.p1,p.p2,p.p3).addAttributes(p.attributes))
                                Create(PolylineShape(p.p3,p.p0,p.p1).addAttributes(p.attributes))
                              }
                            }
                          }
                        })
                        idsForShapesToExplode = idsForShapesToExplode :+ id
                      } else if (x.size == 3) { //Two lines and a polyline.
                        x.foreach(bitSet => {
                          if (firstBitSet.isEmpty) {
                            firstBitSet = Some(bitSet)
                            if (bitSet == 1) { //bitSet 0 is not selected
                              Create(LineShape(p.p1,p.p2).addAttributes(p.attributes))
                              Create(LineShape(p.p2,p.p3).addAttributes(p.attributes))
                              Create(PolylineShape(p.p3,p.p0,p.p1).addAttributes(p.attributes))
                            }
                          } else if (firstBitSet.get == 0) {
                            if (bitSet - firstBitSet.get == 2) { //bitset 1 or 2 is not selected
                              if (bitSet == 2 ) {
                                Create(LineShape(p.p2,p.p3).addAttributes(p.attributes))
                                Create(LineShape(p.p3,p.p0).addAttributes(p.attributes))
                                Create(PolylineShape(p.p0,p.p1,p.p2).addAttributes(p.attributes))
                              } else {
                                Create(LineShape(p.p3,p.p0).addAttributes(p.attributes))
                                Create(LineShape(p.p0,p.p1).addAttributes(p.attributes))
                                Create(PolylineShape(p.p1,p.p2,p.p3).addAttributes(p.attributes))
                              }
                            } else if (bitSet == 3){ //bitset 3 is not selected
                              Create(LineShape(p.p0,p.p1).addAttributes(p.attributes))
                              Create(LineShape(p.p1,p.p2).addAttributes(p.attributes))
                              Create(PolylineShape(p.p2,p.p3,p.p0).addAttributes(p.attributes))
                            }
                          }
                        })
                        idsForShapesToExplode = idsForShapesToExplode :+ id
                      } else  if (x.size == 4) { //Four lines.
                        Create(LineShape(p.p0,p.p1).addAttributes(p.attributes))
                        Create(LineShape(p.p1,p.p2).addAttributes(p.attributes))
                        Create(LineShape(p.p2,p.p3).addAttributes(p.attributes))
                        Create(LineShape(p.p3,p.p0).addAttributes(p.attributes))
                        idsForShapesToExplode = idsForShapesToExplode :+ id
                      } else println("BitSetShapeSelector has 0 bit-sets. This is currently believed not to happen, so explode has not been made able to handle that. Nothing exploded.")
                      somethingExploded = true
                    }
                    case x => println(x)
                  }
                }
                case x => println(x)
              }
            })
          if (somethingExploded == true) {
            //Delete the exploded shapes now - when we're finished manipulating them...
            idsForShapesToExplode.foreach((id) => {
              Delete(id)
            })
            
            Siigna display "explodable shapes in selection exploded"
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