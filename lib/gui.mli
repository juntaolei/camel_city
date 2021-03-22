(** [draw_cell ctx x y c] modifies ctx to draw a cell on cordinates
    ([x], [y]) according to the parameters defined in the GUI config
    [c]. *)
val draw_cell :
  Js_of_ocaml.Dom_html.canvasRenderingContext2D Js_of_ocaml.Js.t ->
  float ->
  float ->
  Types.gui_config ->
  unit

val add_event_listeners :
  Js_of_ocaml.Dom_html.canvasElement Js_of_ocaml.Js.t -> unit

(** [draw_map ctx c] modifies ctx to draw a map according to the
    parameters defined in the GUI config [c]. *)
val draw_map :
  Js_of_ocaml.Dom_html.canvasRenderingContext2D Js_of_ocaml.Js.t ->
  Types.gui_config ->
  unit

(** [reset_canvas ctx c] modifies ctx to clear the canvas according to
    the parameters defined in the GUI config [c]. *)
val reset_canvas :
  Js_of_ocaml.Dom_html.canvasRenderingContext2D Js_of_ocaml.Js.t ->
  Types.gui_config ->
  unit
