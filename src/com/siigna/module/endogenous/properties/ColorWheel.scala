package com.siigna.module.endogenous.properties

import com.siigna.app.view.Graphics
import com.siigna._
import java.awt.Color
import com.siigna.module.endogenous.Menu

/**
 * a wheel to select colors for lines, surfaces and text
 * //TODO: draw the color wheel where the radial menu center is instead centered around the last mouse click.
 * //TODO add color selection functionality
   //TODO: the color wheel only shows after mouse move???
 */

object ColorWheel extends Module {

  private val transp = 1.00f

  //colors, inspired by crayola crayons: http://en.wikipedia.org/wiki/List_of_Crayola_crayon_colors
  lazy val black       = new Color(0.00f, 0.00f, 0.00f, transp)
  lazy val anthracite  = new Color(0.25f, 0.25f, 0.25f, transp)
  lazy val dimgrey     = new Color(0.40f, 0.40f, 0.40f, transp)
  lazy val grey        = new Color(0.60f, 0.60f, 0.60f, transp)
  lazy val darkGrey    = new Color(0.75f, 0.75f, 0.75f, transp)
  lazy val lightGrey   = new Color(0.90f, 0.90f, 0.90f, transp)
  lazy val yellow      = new Color(1.00f, 1.00f, 0.40f, transp)
  lazy val orange      = new Color(1.00f, 0.75f, 0.30f, transp)
  lazy val orangeRed   = new Color(0.95f, 0.45f, 0.22f, transp)
  lazy val red         = new Color(0.95f, 0.12f, 0.30f, transp)
  lazy val radicalRed  = new Color(0.95f, 0.14f, 0.46f, transp)
  lazy val violetRed   = new Color(0.95f, 0.15f, 0.58f, transp)
  lazy val magenta     = new Color(0.95f, 0.15f, 0.80f, transp)
  lazy val plum        = new Color(0.64f, 0.18f, 0.85f, transp)
  lazy val purple      = new Color(0.35f, 0.22f, 0.90f, transp)
  lazy val blue        = new Color(0.12f, 0.25f, 0.95f, transp)
  lazy val navyBlue    = new Color(0.10f, 0.45f, 0.95f, transp)
  lazy val pasificBlue = new Color(0.10f, 0.65f, 0.95f, transp)
  lazy val cyan        = new Color(0.10f, 0.95f, 0.95f, transp)
  lazy val turquise    = new Color(0.10f, 0.95f, 0.75f, transp)
  lazy val caribbean   = new Color(0.10f, 0.95f, 0.50f, transp)
  lazy val green       = new Color(0.10f, 0.95f, 0.10f, transp)
  lazy val lime        = new Color(0.50f, 0.95f, 0.15f, transp)
  lazy val yellowGreen = new Color(0.65f, 0.95f, 0.15f, transp)

  private var activeAngle : Double = 0
  //TODO: hack to prevent module from forwarding to End immediatly.
  //flag to register the first mousedown.
  private var gotMouseDown = false
  private var relativeMousePosition : Option[Vector2D] = None
  private var startPoint : Option[Vector2D] = None

  var activeColor: Option[Color] = None

  def eventHandler = EventHandler(stateMap, stateMachine)

  def radians(rotation : Int) : List[Int] = {

    var i = 0
    var activeRadians = List[Int]()

    //add all radians to a list
    do {

      activeRadians = i :: activeRadians
      i += rotation

    } while (i <= 360)
    activeRadians
  }

  def stateMap = DirectedGraph(

    'Start  ->   'KeyEscape   ->  'End

  )

  def stateMachine = Map(
    //select a color
    'Start -> ((events : List[Event]) => {
      Siigna.navigation = false // Make sure the rest of the program doesn't move
      eventParser.disable // Disable tracking and snapping
      events match {
        case MouseMove(point, _, _) :: tail => relativeMousePosition = Some(point)

        //selects the color to use
        case MouseDown(point, _, _) :: tail => {
          //if the mouse has been pressed once, set the color and go to 'End.
          if (gotMouseDown == true) {
            relativeMousePosition = Some(point)
            //set the color
            if (activeAngle == 0) {activeColor = Some(black)}
            else if (activeAngle == 15) {activeColor = Some(yellowGreen)}
            else if (activeAngle == 30) {activeColor = Some(lime)}
            else if (activeAngle == 45) {activeColor = Some(green)}
            else if (activeAngle == 60) {activeColor = Some(caribbean)}
            else if (activeAngle == 75) {activeColor = Some(turquise)}
            else if (activeAngle == 90) {activeColor = Some(cyan)}
            else if (activeAngle == 105) {activeColor = Some(pasificBlue)}
            else if (activeAngle == 120) {activeColor = Some(navyBlue)}
            else if (activeAngle == 135) {activeColor = Some(blue)}
            else if (activeAngle == 150) {activeColor = Some(purple)}
            else if (activeAngle == 165) {activeColor = Some(plum)}
            else if (activeAngle == 180) {activeColor = Some(magenta)}
            else if (activeAngle == 195) {activeColor = Some(violetRed)}
            else if (activeAngle == 210) {activeColor = Some(radicalRed)}
            else if (activeAngle == 225) {activeColor = Some(red)}
            else if (activeAngle == 240) {activeColor = Some(orangeRed)}
            else if (activeAngle == 255) {activeColor = Some(orange)}
            else if (activeAngle == 270) {activeColor = Some(yellow)}
            else if (activeAngle == 285) {activeColor = Some(lightGrey)}
            else if (activeAngle == 300) {activeColor = Some(darkGrey)}
            else if (activeAngle == 315) {activeColor = Some(grey)}
            else if (activeAngle == 330) {activeColor = Some(dimgrey)}
            else if (activeAngle == 345) {activeColor = Some(anthracite)}
            gotMouseDown = false
            Goto('End)
          }

          else {
            //catch the first mouse down
            startPoint = Some(Menu.oldCenter)
            relativeMousePosition = Some(point)
            gotMouseDown = true
          }
        }
        case _ =>

      }
      //get the current angle from the mouse to the center of the color wheel
      if (relativeMousePosition.isDefined && startPoint.isDefined) {
        val radian = (relativeMousePosition.get - startPoint.get).angle.toInt
        var calculatedAngle = radian * -1 + 450
        if (calculatedAngle > 360)
          activeAngle = calculatedAngle - 360
        else activeAngle = calculatedAngle
          activeAngle = ((activeAngle +7.5)/15).toInt * 15
      }
    }),
    'End -> ((events : List[Event]) => {
      //clear values and reactivate navigation
      startPoint = None
      relativeMousePosition = None
      eventParser.enable
      Siigna.navigation = true
    })
  )
  override def paint(g : Graphics, transform : TransformationMatrix) = {
    if (startPoint.isDefined && relativeMousePosition.isDefined) {
      val sp = startPoint.get.transform(transform)
      val t  = TransformationMatrix(sp,1.3)

      lazy val colorFill = Array(Vector2D(-8.93,75.5),Vector2D(-5.96,75.8),Vector2D(-2.55,76),Vector2D(0,76),Vector2D(2.47,76),Vector2D(6.52,75.7),Vector2D(8.93,75.5),Vector2D(11.9,98.3),Vector2D(9.06,98.6),Vector2D(4.35,98.9),Vector2D(0,99),Vector2D(-4.35,98.9),Vector2D(-9.04,98.6),Vector2D(-11.9,98.3),Vector2D(-8.93,75.5))
      lazy val colorIcon = PolylineShape.fromPoints(Vector2D(-8.93,75.5),Vector2D(-5.96,75.8),Vector2D(-2.55,76.2),Vector2D(0,76.4),Vector2D(2.47,76.2),Vector2D(6.52,75.8),Vector2D(8.93,75.5),Vector2D(11.9,98.3),Vector2D(9.06,98.6),Vector2D(4.35,99.01),Vector2D(0,99.2),Vector2D(-4.35,99.1),Vector2D(-9.04,98.6),Vector2D(-11.9,98.3),Vector2D(-8.93,75.5))
      lazy val colorActive = PolylineShape.fromPoints(Vector2D(-10.93,73.5),Vector2D(-7.96,73.8),Vector2D(-4.55,74),Vector2D(-2,74),Vector2D(4.47,74),Vector2D(8.52,73.7),Vector2D(10.93,73.5),Vector2D(13.9,100.3),Vector2D(11.06,100.6),Vector2D(6.35,100.9),Vector2D(-2,101),Vector2D(-6.35,100.9),Vector2D(-11.04,100.6),Vector2D(-13.9,100.3),Vector2D(-10.93,73.5))

      def drawFill (color : Color, rotation : Int) {

        val fillVector2Ds = colorFill.map(_.transform(t.rotate(rotation)))
        val fillScreenX = fillVector2Ds.map(_.x.toInt).toArray
        val fillScreenY = fillVector2Ds.map(_.y.toInt).toArray

        g setColor color

        g.g.fillPolygon(fillScreenX, fillScreenY, fillVector2Ds.size)
      }

      // Draw the color icons
      drawFill(black,0)
      drawFill(anthracite,345)
      drawFill(dimgrey,330)
      drawFill(grey,315)
      drawFill(darkGrey,300)
      drawFill(lightGrey,285)
      drawFill(yellow,270)
      drawFill(orange,255)
      drawFill(orangeRed,240)
      drawFill(red,225)
      drawFill(radicalRed,210)
      drawFill(violetRed,195)
      drawFill(magenta,180)
      drawFill(plum,165)
      drawFill(purple,150)
      drawFill(blue,135)
      drawFill(navyBlue,120)
      drawFill(pasificBlue,105)
      drawFill(cyan,90)
      drawFill(turquise,75)
      drawFill(caribbean,60)
      drawFill(green,45)
      drawFill(lime,30)
      drawFill(yellowGreen,15)

      //draw a border around the active color
      val distanceToCentre = startPoint.get - relativeMousePosition.get

      if (distanceToCentre.length > 80 && distanceToCentre.length < 130 )  {

      g draw colorIcon.transform(t.rotate(activeAngle-180))
      g draw colorActive.transform(t.rotate(activeAngle-180))

      }
      //draw the color wheel icon outlines
      radians(45).foreach(radian => { g draw colorIcon.transform(t.rotate(radian))})
    }
  }
}