package com.siigna.module.cad.create

import com.siigna._
//import module.porter.DXF.DXFImport
import java.awt.Toolkit
import java.awt.datatransfer.{DataFlavor, StringSelection}
import java.io.{ByteArrayInputStream, InputStream}

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

/**
 * Allow the user to paste - at a specified location - artwork taken from other drawings via the clipboard.
 *
 */

class Paste extends Module {

  //read the clipboard
  //val clip = Toolkit.getDefaultToolkit.getSystemClipboard
  //var clipString = clip.getData(DataFlavor.stringFlavor).toString

  val stateMap: StateMap = Map(
    'Start -> {
      //Exit strategy
      case (End | KeyDown(Key.Esc, _) | End(KeyDown(Key.escape, _)) | MouseDown(_, MouseButtonRight, _) | End(MouseDown(_,MouseButtonRight, _)) ) :: tail => End

      case _ => {
       // val stream : InputStream = new ByteArrayInputStream(clipString.getBytes("UTF-8"))

        //attempt to parse it to Siigna Shapes using the DXF import module in 'Porter
        //DXFImport.parse(stream)

        //TODO: give the user the choice to either position the shapes at a given point or use the default placement

        //create the shapes //TODO: commented out to prevent compile error in Jenkins.
        //if(!DXFImport.shapes.isEmpty) Create(DXFImport.shapes)
        //DXFImport.shapes = List() //clean up
        End //exit
      }

    }
  )
}