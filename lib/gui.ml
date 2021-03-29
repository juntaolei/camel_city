open Js_of_ocaml
open Types
module Html = Dom_html

let map_canvas =
  match Html.getElementById_coerce "gui_map" Html.CoerceTo.canvas with
  | None -> raise Not_found
  | Some canvas -> canvas

let map_ctx = map_canvas##getContext Html._2d_

let fg_canvas =
  match
    Html.getElementById_coerce "gui_overlay" Html.CoerceTo.canvas
  with
  | None -> raise Not_found
  | Some canvas -> canvas

let fg_ctx = fg_canvas##getContext Html._2d_

let reset_canvas ctx state =
  ctx##clearRect 0. 0.
    (state |> canvas_size |> fst |> float_of_int)
    (state |> canvas_size |> snd |> float_of_int)

let setup_canvas
    (canvas : Html.canvasElement Js.t)
    (ctx : Html.canvasRenderingContext2D Js.t)
    state =
  canvas##.width := state |> canvas_size |> fst;
  canvas##.height := state |> canvas_size |> snd;
  ctx##translate
    (float_of_int canvas##.width /. 2.)
    (state |> cell_size |> snd |> float_of_int)

let draw_cell (ctx : Html.canvasRenderingContext2D Js.t) state x y color
    =
  let cell_width = state |> cell_size |> fst in
  let cell_height = state |> cell_size |> snd in
  ctx##save;
  ctx##translate
    ((float_of_int y -. float_of_int x) *. float_of_int cell_width /. 2.)
    ((float_of_int x +. float_of_int y)
    *. float_of_int cell_height
    /. 2.);
  ctx##beginPath;
  ctx##moveTo 0. 0.;
  ctx##lineTo
    (float_of_int cell_width /. 2.)
    (float_of_int cell_height /. 2.);
  ctx##lineTo 0. (float_of_int cell_height);
  ctx##lineTo
    (-.float_of_int cell_width /. 2.)
    (float_of_int cell_height /. 2.);
  ctx##closePath;
  ctx##.fillStyle := Js.string color;
  ctx##fill;
  ctx##restore

let create_img filename =
  let img = Html.createImg Html.document in
  img##.src := Js.string filename;
  img

let draw_img state x y texture =
  let cell_width = state |> cell_size |> fst in
  let cell_height = state |> cell_size |> snd in
  map_ctx##save;
  map_ctx##translate
    ((float_of_int y -. float_of_int x) *. float_of_int cell_width /. 2.)
    ((float_of_int x +. float_of_int y)
    *. float_of_int cell_height
    /. 2.);
  map_ctx##drawImage_full (create_img texture) 0. 0. 130. 230. (-65.) 0.
    130. 230.;
  map_ctx##restore

let draw_map state =
  Array.iteri
    (fun i ->
      Array.iteri (fun j _ -> draw_img state i j "textures/sand.png"))
    (cells state)

let cell_positions state event =
  (* print_endline (string_of_int (event##.clientX - int_of_float
     fg_canvas##getBoundingClientRect##.left)); *)
  let mouse_x =
    event##.clientX
    - int_of_float fg_canvas##getBoundingClientRect##.left
  in
  let mouse_y =
    event##.clientY
    - int_of_float fg_canvas##getBoundingClientRect##.top
  in
  let canvas_width = state |> canvas_size |> fst in
  let cell_width = state |> cell_size |> fst in
  let cell_height = state |> cell_size |> snd in
  ( (mouse_x - (canvas_width / 2)) / cell_width,
    (mouse_y - cell_height) / cell_height )

let highlight state event =
  let canvas_width = state |> canvas_size |> fst in
  let canvas_height = state |> canvas_size |> snd in
  let map_length = state |> map_length in
  let positions = cell_positions state event in
  print_endline
    ((positions |> fst |> string_of_int)
    ^ " "
    ^ (positions |> snd |> string_of_int));
  fg_ctx##clearRect
    (float_of_int (-canvas_width))
    (float_of_int (-canvas_height))
    (float_of_int (canvas_width * 2))
    (float_of_int (canvas_height * 2));
  if
    fst positions >= 0
    && snd positions < map_length
    && fst positions >= 0
    && snd positions < map_length
  then
    draw_cell fg_ctx state (fst positions) (snd positions) "lightgray";
  Js._false

let draw_gui state =
  reset_canvas map_ctx state;
  reset_canvas fg_ctx state;
  setup_canvas map_canvas map_ctx state;
  setup_canvas fg_canvas fg_ctx state;
  draw_map state;
  Html.addEventListener fg_canvas Html.Event.mouseover
    (Dom.handler (highlight state))
    Js._true
  |> ignore
