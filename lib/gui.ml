open Js_of_ocaml
open State
module Html = Dom_html

(** Module wide value for the background canvas. *)
let bg_canvas =
  match Html.getElementById_coerce "bg" Html.CoerceTo.canvas with
  | None -> raise Not_found
  | Some canvas -> canvas

(** Module wide value for the 2D context of the background canvas. *)
let bg_ctx = bg_canvas##getContext Html._2d_

(** Module wide value for the foreground canvas. *)
let fg_canvas =
  match Html.getElementById_coerce "fg" Html.CoerceTo.canvas with
  | None -> raise Not_found
  | Some canvas -> canvas

(** Module wide value for the 2D context of the foreground canvas. *)
let fg_ctx = fg_canvas##getContext Html._2d_

(** Module wide value for the progress HTML element for showing the
    amount of money. *)
let money_progres_bar = Html.getElementById "money"

(** Module wide value for the progress HTML element for showing the
    amount of food. *)
let food_progres_bar = Html.getElementById "food"

(** List of texture names to be used in the GUI. *)
let texture_names = [ "sand" ]

(** [create_img filename] is the image loaded based on a [filename]. *)
let create_img filename =
  let img = Html.createImg Html.document in
  img##.src := Js.string ("textures/" ^ filename ^ ".png");
  img

(** List of loaded textures to be used in the GUI. *)
let textures = List.map (fun x -> (x, create_img x)) texture_names

(** [find_texture name] is the loaded texture given by the texture
    [name]. *)
let find_texture name =
  List.find (fun x -> name = fst x) textures |> snd

(** [set_progress_bar state element] changes the attributes of [element]
    to show some statistics about state. *)
let set_progress_bar state element =
  element##setAttribute (Js.string "max") (Js.string "100");
  element##setAttribute (Js.string "value") (Js.string "10")

(** [set_money_progress_bar state] changes the attributes of the
    progress HTML element to show money. *)
let set_money_progress_bar state =
  set_progress_bar state money_progres_bar

(** [set_food_progress_bar state] changes the attributes of the progress
    HTML element to show food. *)
let set_food_progress_bar state =
  set_progress_bar state food_progres_bar

(** [reset_canvas state] clears everything from the foreground canvas
    and the background canvas.*)
let reset_canvas state =
  bg_ctx##clearRect 0. 0.
    (state |> canvas_size |> fst |> float_of_int)
    (state |> canvas_size |> snd |> float_of_int);
  fg_ctx##clearRect 0. 0.
    (state |> canvas_size |> fst |> float_of_int)
    (state |> canvas_size |> snd |> float_of_int)

(** [setup_canvas state] setups the foreground canvas and the background
    canvas based on properties from [state]. *)
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

let reset_gui state = reset_canvas state

let setup_gui state =
  setup_canvas state;
  set_money_progress_bar state;
  set_food_progress_bar state

(** [draw_cell state x y color] colors a cell of indices [x] and [y] in
    [state] with [color] on the foreground canvas. *)
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

(** [draw_img state x y texture] draws the [texture] that represents a
    cell of indices [x] and [y] in [state] on the background canvas. *)
let draw_img state x y texture =
  let cell_width = state |> cell_size |> fst in
  let cell_height = state |> cell_size |> snd in
  bg_ctx##save;
  bg_ctx##translate
    ((float_of_int y -. float_of_int x) *. float_of_int cell_width /. 2.)
    ((float_of_int x +. float_of_int y)
    *. float_of_int cell_height
    /. 2.);
  bg_ctx##drawImage_full texture 0. 0. 130. 230. (-65.) 0. 130. 230.;
  bg_ctx##restore

(** [draw_map state] draws the cells in [state] on the background
    canvas.*)
let draw_map state =
  Array.iteri
    (fun i ->
      Array.iteri (fun j _ -> draw_img state i j (find_texture "sand")))
    (cells state)

(** [cell_positions state event] are the x and y indices of a cell in
    [state] that the mouse through [event] is currently hovering over. *)
let cell_positions state event =
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

(** [highlight state event] highlights a cell in [state] by calculating
    its positions with [event]. *)
let highlight state (event : Html.mouseEvent Js.t) =
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
  then
    draw_cell state (fst positions) (snd positions)
      "hsla(60, 100%, 50%, 0.25)";
  Js._true

let add_event_listeners state =
  Html.addEventListener Html.document Html.Event.mousemove
    (Dom.handler (highlight state))
    Js._false
  |> ignore

let draw_gui state = draw_map state
