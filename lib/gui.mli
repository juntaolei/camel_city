(** [reset_canvas state] clears everything from the foreground canvas
    and the background canvas.*)
val reset_canvas : State.state -> unit

(** [setup_canvas state] setups the foreground canvas and the background
    canvas based on properties from [state]. *)
val setup_canvas : State.state -> unit

(** [draw_gui state] draws the GUI of the game based on a given [state]. *)
val draw_gui : State.state -> unit

val highlight :
  State.state ->
  Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t ->
  bool Js_of_ocaml.Js.t
