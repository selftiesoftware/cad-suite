package com.siigna.module.base.create

import com.siigna._

/**
A freehand Artline pen
 */

object Artline extends Module{

    var currentMouse = Vector2D(0,0)

    //val buffer = new scala.collection.mutable.ListBuffer[Vector2D]
    val eventHandler = EventHandler(stateMap, stateMachine)
    var dotshape = Vector2D(10,10)
    var points = List[Vector2D]()
    var startPoint : Option[Vector2D] = None
    var currentPoint : Option[Vector2D] = None
    var shape : PolylineShape = PolylineShape.empty

    def hasPoint = (points.size >= 1)

    def stateMap = DirectedGraph(
      'Start         -> 'KeyEscape ->         'End,
      'Start         -> 'MouseMove   ->       'Points
    )

    def stateMachine = Map(
      'Start -> ((events : List[Event]) => {
        events match {
          case MouseUp(_, _, _) :: tail => {
          }
          case _ =>
        }
      None
      }),
      //draw single points on mouseclick
      'Points -> ((events : List[Event]) => {
         events match {
            case MouseDown(p, _, _) :: tail => {
              startPoint = Some(p)
              //points = List(LineShape(dotshape, point),LineShape(dotshape, point))
              //Message(points)
              //println(points)
            }
            case MouseDrag(p, _, _) :: tail => {
              currentPoint = Some(p)
              if (startPoint.get.distanceTo(currentPoint.get) > 5) {
                points = points :+ startPoint.get
                startPoint = currentPoint
               //draw a polyline from the points saved in shape
               shape =  PolylineShape.fromPoints(points)
              }
            }
            case MouseUp(_, MouseButtonRight, _) :: tail => Goto('End)
            case MouseUp(p, _, _) :: tail => {
              points = points :+p
              shape = PolylineShape.fromPoints(points)
              Create(shape)
              //clear the list
              shape = PolylineShape.empty
              points = List[Vector2D]()
              Goto('Start)
            }
            case _ => None
         }
      None
      }),

      'End -> ((events : List[Event]) => {
        events match {
          case KeyDown(Key.Escape, _) :: tail => None
          case _ => {
            if (points.size > 0){
            //draw a polyline from the points saved in the path
            //shape = PolylineShape.fromPoints(points)
            //Create(shape)
            }
          }
        com.siigna.module.base.Default.previousModule = Some('Artline)
        }
        None
      })
    )
    override def paint(g : Graphics, t : TransformationMatrix) {
      if (points.length > 0 && currentPoint.isDefined) {
        g draw shape.transform(t)
        g draw LineShape(currentPoint.get,points.last).transform(t)
      }

    }
}