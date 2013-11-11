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

import com.siigna._
import app.Siigna
import app.model.shape.PolylineShape.PolylineShapeOpen
import module.cad.create.InputRequest
import module.Tooltip

class Join extends Module{

  /**
   * a function to join the lines and polylines which can be joined within a list of shapes.
   * input: a list of shapes
   * return: the joined shapes
   * old shapes are deleted.
   */



  //STRATEGY:
  //1) if sel.size == 2
  //2) get endPoints
  //3) if coinsiding ends: Create(newS = s1+s2)
  //4)del s1 and s2
  //5) select newS
  //6) Start(Select)

  //check if two shapes have coinsiding ends
  def endsCheck(s1 : Shape, s2 : Shape) : Boolean = {
    val start1 = epsilon(s1.geometry.vertices.head)
    val end1 = epsilon(s1.geometry.vertices.last)
    val start2 = epsilon(s2.geometry.vertices.head)
    val end2 = epsilon(s2.geometry.vertices.last)

    //check for coinsiding ends and join shapes if such are found
    if(start1 == start2 || start1 == end2 || end1 == start2 || end2 == start2) {
      true
    } else {
      false
    }
  }

  //a function to round a vector2D to a tolerance
  def epsilon (v : Vector2D) : Vector2D = Vector2D(math.round(v.x * 100000)/100000.toDouble,math.round(v.y * 100000)/100000.toDouble)

  //a function to merge two shapes if an end coinside.
  //returns None if this is not the case
  def addTwoVerticeLists(l1 : List[Vector2D], l2 : List[Vector2D]) : List[Vector2D] = {
    val s1 = l1.head
    val e1 = l1.last
    val s2 = l2.head
    val e2 = l2.last

    //catch lists which will result in closed polylines
    if((s1 == e2 && s2 == e1) || (s1 == s2 && e2 == e1)) {
      if(e1 == s2) l1 ++ l2 else l1.reverse ++ l2
    }
    //catch lists which will result in open polylines
    else {
      //align list1
      val newL1 = if(s1 == s2) {
        l1.reverse
      } else l1

      //align list2
      val newL2 = if(e1 == e2) {
        l2.reverse
      } else l2

      newL1 ++ newL2
      if(s1 == e2) newL1.reverse ++ newL2.reverse else newL1 ++ newL2 //prevent false construction of closed PLs
    }
  }

  def m = mousePosition.transform(View.deviceTransformation)


  def nearestShape : Option[(Int, Shape)] = {
    val drawing = Drawing(m)
    if (!drawing.isEmpty) {
      Some(drawing.reduceLeft((a, b) => if (a._2.distanceTo(m) < b._2.distanceTo(m)) a else b))
    } else None
  }

  def shapeWithinSelectionDistance: Boolean = {
    if (nearestShape.isDefined) {
      if (nearestShape.get._2.distanceTo(m) < Siigna.selectionDistance) true
      else false
    } else {
      false
    }
  }

  var selection = Selection()

  var selectIDs : List[Int] = List()

  var s1vertices : List[Vector2D] = List()
  var s2vertices : List[Vector2D] = List()

  val stateMap: StateMap = Map(

    'Start -> {
      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => {
        Deselect()
        End
      }
      case MouseDown(p, MouseButtonRight, _) :: tail => {
        Deselect()
        End
      }
      case End(KeyDown(Key.Esc, _)) :: tail => {
        Deselect()
        End
      }
      case End(MouseDown(p, MouseButtonRight, _)) :: tail => {
        Deselect()
        End
      }

      //look for a mouse point indicating that another shape is chosen for join evaluation
      case MouseDown(p, MouseButtonLeft, _) :: tail => {
        if (!shapeWithinSelectionDistance) Deselect()
        else {
          val (id, shape) = nearestShape.get
          selectIDs = selectIDs :+ id
          //Deselect()
          Select(selectIDs)
        }
        if(Drawing.selection.size >= 2) 'Join else 'Start
      }

      //else do nothing
      case _ => {
        Tooltip.updateTooltip(List("click to join shapes. Right click or ESC to exit"))

        if(Drawing.selection.size >= 2) {
          'Join
        }
      }
    },

    'Join -> {
      //exit strategy
      case KeyDown(Key.Esc, _) :: tail => End
      case MouseDown(p, MouseButtonRight, _) :: tail => End
      case End(KeyDown(Key.Esc, _)) :: tail => End
      case End(MouseDown(p, MouseButtonRight, _)) :: tail => End

      case _ => {


        val selectionShapes = Drawing.selection.shapes.map(s => s._2)

        if(selectionShapes.size == 2) {
          selection = Drawing.selection //save the selection so that the original shapes can be deleted

          Siigna display "joining shapes"

          //2) get endPoints
          val s1 = selectionShapes.head
          val s2 = selectionShapes.last

          //check that the shapes are line or polyline shapes
          if(s1.isInstanceOf[LineShape] || s1.isInstanceOf[PolylineShapeOpen]) {
            if(s2.isInstanceOf[LineShape] || s2.isInstanceOf[PolylineShapeOpen]) {

              //if ends coinside, join the shapes
              if(endsCheck(s1,s2)) {
                //get the vertices of line one
                s1 match {
                  case l : LineShape => s1vertices = List(l.p1,l.p2)
                  case p : PolylineShapeOpen => s1vertices = p.geometry.vertices.toList
                  case _ => {
                    Siigna display "Oooops, something wrong in join with shape 1"
                    End
                  }
                }

                //get the vertices of line two
                s2 match {
                  case l : LineShape => s2vertices = List(l.p1,l.p2)
                  case p : PolylineShapeOpen => s2vertices = p.geometry.vertices.toList
                  case _ => {
                    Siigna display "Oooops, something wrong in join with shape 2"
                    End
                  }
                }
                //join (poly)lines
                val s = addTwoVerticeLists(s1vertices,s2vertices)
                if(!s.isEmpty) Create(PolylineShape(s))
                //5)del s1 and s2
                if(!selection.isEmpty) Delete(selection)
                Deselect()
                //6) select newS
                val id = if(Siigna.latestID.isDefined) Siigna.latestID else None
                if(id.isDefined) Select(id)

                //7) Start(Select) to look for new shapes to join
                Siigna display("lines joined. Click to join more lines")
                'Start
              } else {
              Siigna display "shapes cannot be joined"
              selectIDs = List()
              selection = Selection()
              Deselect()
              'Start
              }
            }
          }
        }
        else if (selectionShapes.size > 2) {
          Siigna display ("joining of more than two shapes not implemented yet!")
          selectIDs = List()
          selection = Selection()
          Deselect()
          End


        // if sel.size == 2
        } else {
          Siigna display "Select two objects to join"
          Start('cad, "Selection")
        }
        //End the module
        //End
      }
    }
  )
}
