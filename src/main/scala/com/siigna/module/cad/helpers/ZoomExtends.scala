package com.siigna.module.cad.helpers

import com.siigna._
import module.Tooltip

class ZoomExtends extends Module {

  val stateMap : StateMap = Map(

    'Start-> {
      case _ => {
        View.zoomExtends
        Siigna display "Zooming to extends"
        Tooltip.blockUpdate(3500)
        End
      }
    }
  )
}