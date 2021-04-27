(** [reset_gui state] clears the HTML document based on properties from
    [state]. *)
val reset_gui : State.state -> unit

(** [setup_gui state] setups the HTML document based on properties from
    [state]. *)
val setup_gui : State.state -> unit

(** [add_event_listeners state] adds event listeners to the HTML
    document. *)
val add_event_listeners : State.state -> unit

(** [draw_gui state] draws the GUI of the game based on a given [state]. *)
val draw_gui : State.state -> unit

val draw_building_selections : unit
