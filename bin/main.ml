open Js_of_ocaml
open Lib
module Html = Dom_html

let js = Js.string

let document = Html.document

let onload _ =
  let gui =
    Js.Opt.get
      (document##getElementById (js "gui"))
      (fun _ -> assert false)
  in
  let config = Types.new_config 1200. 1200. 20 20 48. 24. "white" in
  let canvas = Html.createCanvas document in
  canvas##.width := int_of_float (Types.canvas_width config);
  canvas##.height := int_of_float (Types.canvas_height config);
  Dom.appendChild gui canvas;
  let ctx = canvas##getContext Html._2d_ in
  Gui.draw_map ctx config;
  print_endline "Pass";
  Js._false

let _ = Html.window##.onload := Html.handler onload
