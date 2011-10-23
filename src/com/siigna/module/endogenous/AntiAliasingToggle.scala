package com.siigna.module.endogenous

import com.siigna._

/**
 * Toggles Anti-aliasing on/off.
 */
object AntiAliasingToggle extends Module {

  val eventHandler = EventHandler(stateMap, stateMachine)

  lazy val stateMap = DirectedGraph (
    'Start    -> 'Event     -> 'End
  )

  lazy val stateMachine = Map(

    'Start -> ((events : List[Event]) => {
      Goto('End)
      None
    }),

    'End -> ((events : List[Event]) => {
      Preferences.toggle("anti-aliasing");
      None
    })
  )
}