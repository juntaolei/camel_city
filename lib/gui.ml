open Js_of_ocaml
open Types
module Html = Dom_html

let bg_canvas =
  match Html.getElementById_coerce "bg" Html.CoerceTo.canvas with
  | None -> raise Not_found
  | Some canvas -> canvas

let bg_ctx = bg_canvas##getContext Html._2d_

let fg_canvas =
  match Html.getElementById_coerce "fg" Html.CoerceTo.canvas with
  | None -> raise Not_found
  | Some canvas -> canvas

let fg_ctx = fg_canvas##getContext Html._2d_

let reset_canvas state =
  bg_ctx##clearRect 0. 0.
    (state |> canvas_size |> fst |> float_of_int)
    (state |> canvas_size |> snd |> float_of_int);
  fg_ctx##clearRect 0. 0.
    (state |> canvas_size |> fst |> float_of_int)
    (state |> canvas_size |> snd |> float_of_int)

let setup_canvas state =
  bg_canvas##.width := state |> canvas_size |> fst;
  bg_canvas##.height := state |> canvas_size |> snd;
  fg_canvas##.width := state |> canvas_size |> fst;
  fg_canvas##.height := state |> canvas_size |> snd;
  bg_ctx##translate
    (float_of_int bg_canvas##.width /. 2.)
    (state |> cell_size |> snd |> float_of_int);
  fg_ctx##translate
    (float_of_int fg_canvas##.width /. 2.)
    (state |> cell_size |> snd |> float_of_int)

let draw_cell state x y color =
  let cell_width = state |> cell_size |> fst in
  let cell_height = state |> cell_size |> snd in
  fg_ctx##save;
  fg_ctx##translate
    ((float_of_int y -. float_of_int x) *. float_of_int cell_width /. 2.)
    ((float_of_int x +. float_of_int y)
    *. float_of_int cell_height
    /. 2.);
  fg_ctx##beginPath;
  fg_ctx##moveTo 0. 0.;
  fg_ctx##lineTo
    (float_of_int cell_width /. 2.)
    (float_of_int cell_height /. 2.);
  fg_ctx##lineTo 0. (float_of_int cell_height);
  fg_ctx##lineTo
    (-.float_of_int cell_width /. 2.)
    (float_of_int cell_height /. 2.);
  fg_ctx##lineTo 0. 0.;
  fg_ctx##closePath;
  fg_ctx##.fillStyle := Js.string color;
  fg_ctx##fill;
  fg_ctx##restore

let create_img filename =
  let img = Html.createImg Html.document in
  img##.src := Js.string filename;
  img

let draw_img state x y texture =
  let cell_width = state |> cell_size |> fst in
  let cell_height = state |> cell_size |> snd in
  bg_ctx##save;
  bg_ctx##translate
    ((float_of_int y -. float_of_int x) *. float_of_int cell_width /. 2.)
    ((float_of_int x +. float_of_int y)
    *. float_of_int cell_height
    /. 2.);
  bg_ctx##drawImage_full (create_img texture) 0. 0. 130. 230. (-65.) 0.
    130. 230.;
  bg_ctx##restore

let draw_map state =
  Array.iteri
    (fun i ->
      Array.iteri (fun j _ -> draw_img state i j "textures/sand.png"))
    (cells state)

let cell_positions state event =
  (* print_endline (string_of_int (event##.clientX - int_of_float
     fg_canvas##getBoundingClientRect##.left)); *)
  let mouse_x =
    float_of_int event##.clientX
    -. fg_canvas##getBoundingClientRect##.left
  in
  let mouse_y =
    float_of_int event##.clientY
    -. fg_canvas##getBoundingClientRect##.top
  in
  let cell_width = state |> cell_size |> fst |> float_of_int in
  let cell_height = state |> cell_size |> snd |> float_of_int in
  let map_length = state |> map_length |> float_of_int in
  let x = (mouse_x /. cell_width) -. (map_length /. 2.) in
  let y = (mouse_y -. cell_height) /. cell_height in
  (y -. x |> floor |> int_of_float, y +. x |> floor |> int_of_float)

let highlight state event =
  reset_canvas state;
  let canvas_width = state |> canvas_size |> fst in
  let canvas_height = state |> canvas_size |> snd in
  let map_length = state |> map_length in
  let positions = cell_positions state event in
  fg_ctx##clearRect
    (float_of_int (-canvas_width))
    (float_of_int (-canvas_height))
    (float_of_int (canvas_width * 2))
    (float_of_int (canvas_height * 2));
  if
    fst positions >= 0
    && fst positions < map_length
    && snd positions >= 0
    && snd positions < map_length
  then draw_cell state (fst positions) (snd positions) "blue";
  Js._true

let draw_gui state =
  draw_map state;
  let wrapper event = highlight state event in
  Html.addEventListener Html.document Html.Event.mousemove
    (Dom.handler wrapper) Js._false
  |> ignore
