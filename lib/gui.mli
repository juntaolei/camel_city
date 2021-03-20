val draw_cell :
  Js_of_ocaml.Dom_html.canvasRenderingContext2D Js_of_ocaml.Js.t ->
  float ->
  float ->
  Types.gui_config ->
  unit

val draw_map :
  Js_of_ocaml.Dom_html.canvasRenderingContext2D Js_of_ocaml.Js.t ->
  Types.gui_config ->
  unit

val reset_canvas :
  Js_of_ocaml.Dom_html.canvasRenderingContext2D Js_of_ocaml.Js.t ->
  Types.gui_config ->
  unit
