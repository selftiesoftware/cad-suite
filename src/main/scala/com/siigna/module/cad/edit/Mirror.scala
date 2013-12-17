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

package com.siigna.module.cad.edit

import com.siigna.module.{ModuleInit, Module}
import com.siigna._
import com.siigna.util.event.{End, MouseDown, KeyDown}
import com.siigna.module.cad.create.{DynamicDrawFromDouble, InputRequest, DynamicDrawFromVector2D}
import java.awt.geom.AffineTransform

class Mirror extends Module {

  var endPoint : Option[Vector2D] = None
  var startPoint : Option[Vector2D] = None
  var shapes : List[Shape] = List()
  def transformSelection(t : TransformationMatrix) = Drawing.selection.transform(t).shapes.values
  val origin = Drawing.selection.transformation

  /**
   * mirror a list of shapes around an arbitrary line
   * @param p1 first point on mirror line
   * @param p2 second point on mirror line
   * @return the transformed shapes
   */
  def mirror(p1 : Vector2D, p2 : Vector2D) : List[Shape] = {

    //get the rotation angle from the two points set
    val refLine = Segment2D(p1,p2).transform(TransformationMatrix(-p1,1))
    val refAngle = refLine.p2.angle

    Drawing.selection.transform(TransformationMatrix(-p1,1)) //Move to 0,0
    Drawing.selection.transform(TransformationMatrix( ).rotate(-refAngle, p1)) //rotate to horizontal

    //From here on transformations are done to a list, because I cound not get .flipY to work with the selection.
    val mirrored = Drawing.selection.shapes.map(s =>  s._2.transform(TransformationMatrix( ).flipY)).toList //mirror
    val mirrored2 = mirrored.map(s => s.transform(TransformationMatrix( ).rotate(refAngle))) //rotate back
    //return:
    mirrored2.map(s => s.transform(TransformationMatrix(p1,1))) //move back
  }


val stateMap: StateMap = Map(

    //find the first mirror point
    'Start -> {
      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      case MouseDown(p, MouseButtonRight, _) :: tail => End
      case End(KeyDown(Key.Esc, _)) :: tail => End
      case End(MouseDown(p, MouseButtonRight, _)) :: tail => End

      case End(p : Vector2D) :: tail => {
        startPoint = Some(p)
        println("got startPoint: "+p)
        'Mirror
      }
      //look for the first point on the mirror line
      case _ => {
        if(!Drawing.selection.isEmpty) {
          shapes = Drawing.selection.shapes.map(s => s._2).toList
          Siigna display "set start point of mirror line"
          Start('cad, "create.Input", InputRequest(6,None))
        }
        else {
          Siigna display "select shapes to mirror"
          End
        }
      }
    },
     //find the second mirror point
    'Mirror -> {
      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      case MouseDown(p, MouseButtonRight, _) :: tail => End
      case End(KeyDown(Key.Esc, _)) :: tail => End
      case End(MouseDown(p, MouseButtonRight, _)) :: tail => End

      case MouseMove(p, _, _) :: tail => {
      Siigna display "set end point of mirror line"
      Start('cad, "create.Input", InputRequest(6,None))
      }
      case End(p : Vector2D) :: tail => {
        endPoint = Some(p)
        Siigna display ("mirrored "+shapes.length+ " shape(s)")
        val newShapes = mirror(startPoint.get,endPoint.get)
        Delete(Drawing.selection)
        Create(newShapes)
        End
      }
      case _ => //wait for input
    },
    'End -> {
      case _ => End
    }
  )
  override def paint(g: Graphics, t: TransformationMatrix) {
    //draw mirror line dynamically
    if(startPoint.isDefined && !endPoint.isDefined) g draw LineShape(startPoint.get.transform(t),mousePosition)
  }
}
