/* 2010 (C) Copyright by Siigna, all rights reserved. */

package com.siigna.module.endogenous

import java.awt.{Color, Graphics => AWTGraphics, Graphics2D, RenderingHints}
import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage
import java.awt.print.{PageFormat, Paper, Printable, PrinterJob}

import com.siigna._

class Print extends Module {

  lazy val eventHandler = EventHandler(stateMap, stateMachine)

  lazy val stateMap     = DirectedGraph('Start     -> 'KeyEscape -> 'End)

  lazy val stateMachine = Map(
    'Start -> ((events : List[Event]) => {
      try {
        val printerJob = PrinterJob.getPrinterJob
        val pageFormat = printerJob.defaultPage

        printerJob.setJobName("Siigna Printout");
        printerJob.setCopies(1);

        if (Model.boundary.width >= Model.boundary.height) {
          // The origin is at the bottom left of the paper with x running bottom to top and y running left to right.
          pageFormat.setOrientation(PageFormat.LANDSCAPE)
        } else {
          // The origin is at the top left of the paper with x running to the right and y running down the paper.
          pageFormat.setOrientation(PageFormat.PORTRAIT)
        }

        val paper = new Paper()
        val margin = Siigna.printMargin
        paper.setImageableArea(margin, margin, paper.getWidth() - margin * 2, paper.getHeight() - margin * 2);
        pageFormat.setPaper(paper)

        printerJob.setPrintable(new MyPrintable, pageFormat)

        if (printerJob.printDialog() != false) {// choose printer
            printerJob.print()
        }

      } catch {
        case e => println(e)
      }

      Goto('End)
    }),
    'End   -> ((events : List[Event]) => { })
  )

}

class MyPrintable extends Printable {
  def print(g : AWTGraphics, pageFormat : PageFormat, pageIndex : Int) = {
    if (pageIndex != 0) {
      Printable.NO_SUCH_PAGE
    } else {
      val graphics2D = g.asInstanceOf[Graphics2D]
      val graphics   = new Graphics(graphics2D)

      // Define the boundary for the print
      val boundary     = if (Model.boundary.width <= Model.boundary.height)
          Rectangle(Vector(pageFormat.getImageableX,   pageFormat.getImageableY),
                    Vector(pageFormat.getImageableWidth, pageFormat.getImageableHeight))
        else
          Rectangle(Vector(pageFormat.getImageableY, pageFormat.getImageableX),
                    Vector(pageFormat.getImageableWidth, pageFormat.getImageableHeight))

      val bufferedImage = new BufferedImage(boundary.width.toInt, boundary.height.toInt, BufferedImage TYPE_INT_RGB)

      // Define the buffer graphics as an instance of <code>Graphics2D</code>
      // (which is much nicer than just <code>Graphicsc</code>).
      val bufferedGraphics2D = bufferedImage.getGraphics.asInstanceOf[Graphics2D]

      // Wraps the graphics-object in our own Graphics-wrapper (more simple API).
      val bufferedGraphics = new Graphics(bufferedGraphics2D)

      // Now clear the view;
      bufferedGraphics2D setBackground("#DDDDDF".color)
      bufferedGraphics2D clearRect(0, 0, boundary.width.toInt, boundary.height.toInt)

      // Draw a white rectangle inside the boundary of the current model.
      bufferedGraphics2D setBackground(Color white)
      bufferedGraphics2D clearRect(boundary.topLeft.x.toInt, boundary.topLeft.y.toInt - boundary.height.toInt, boundary.width.toInt, boundary.height.toInt)

      // Enable anti-aliasing.
      bufferedGraphics2D setRenderingHint(RenderingHints KEY_ANTIALIASING, RenderingHints VALUE_ANTIALIAS_ON)

      // Create a new transformation-matrix for the model
      // TODO: Get the correct screenToPaperScale
      val screenToPaperScale = 2.845 / Model.boundaryScale
      val t1 = TransformationMatrix(-Model.boundary.topLeft, 1)
      val t2 = TransformationMatrix(Vector(0, 0), 1).flipY.scale(screenToPaperScale)

      // Iterate through the dom and draw the shape on the buffer graphics
      Model map( _.transform(t1).transform(t2) ) foreach( bufferedGraphics draw(_))

      // Paint filters and interfaces accesible by the modules.
      // TODO: Test matrixes
      Siigna.interface.paint(bufferedGraphics, t1.concatenate(t2))

      //val outline = PolylineShape.fromRectangle(boundary)

      graphics2D.drawImage(bufferedImage, 0, 0, null)
      Printable.PAGE_EXISTS
    }
  }
}