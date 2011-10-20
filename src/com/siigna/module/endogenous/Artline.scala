package com.siigna.module.endogenous

import com.siigna._
import org.omg.PortableInterceptor.NON_EXISTENT
import scala.Predef._

/**
A freehand Artline pen
 */

class Artline extends Module{

    val eventHandler = EventHandler(stateMap, stateMachine)
    var dotshape = Vector(10,10)
    var points : List[LineShape] = List[LineShape]()
    var point : Vector = Vector(0,0)
    var path = List[Vector]()

    def hasPoint = (points.size >= 1)

    override def paint(g : Graphics, t : TransformationMatrix) {
      if (hasPoint) {
        g draw points
      }
    }

    def stateMap = DirectedGraph(
      'Start         -> 'KeyEscape ->         'End,
      'Start         -> 'MouseMove   ->       'Points
    )

    def stateMachine = Map(
      'Start -> ((events : List[Event]) => {
        events match {
          case MouseUp(_, _, _) :: tail => {
            println("starting module")
          }
          case _ =>
        }
      None
      }),
      //draw single points on mouseclick
      'Points -> ((events : List[Event]) => {
         events match {
            case MouseDown(point, _, _) :: tail => {

              points = List(LineShape(dotshape, point),LineShape(dotshape, point))

              ForwardTo('Point)
              Message(points)
              println(points)
            }
            case _ => None
         }
        println("exit nodegenerator")
      None
      }),

      'Lines -> ((events : List[Event]) => {
         events match {
           case MouseDrag(_, _, _) :: tail => {
             println("draw connected lines here")
           }
           case _ =>
         }
      }),

      'End -> ((events : List[Event]) => {
        events match {
          case KeyDown(Key.Escape, _) :: tail => None
          case _ => {

            Goto ('Points)
          }
        }
        None
        //Create(LineShape(ink.head,ink.head + Vector(3,0)))
      })
    )
}