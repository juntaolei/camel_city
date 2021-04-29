open Buildings
open Js_of_ocaml
open State
module Html = Dom_html

(** [get_element_by_id name element_type] gets an HTML element of
    [element_type] by its [id]. *)
let get_element_by_id id element_type =
  match Html.getElementById_coerce id element_type with
  | None -> raise Not_found
  | Some element -> element

(** Module wide value for the background canvas. *)
let bg_canvas = get_element_by_id "bg" Html.CoerceTo.canvas

(** Module wide value for the 2D context of the background canvas. *)
let bg_ctx = bg_canvas##getContext Html._2d_

(** Module wide value for the foreground canvas. *)
let fg_canvas = get_element_by_id "fg" Html.CoerceTo.canvas

(** Module wide value for the 2D context of the foreground canvas. *)
let fg_ctx = fg_canvas##getContext Html._2d_

(** Module wide value for the HTML progress element for showing the
    amount of money. *)
let money_progres_bar = Html.getElementById "money"

(** Module wide value for the HTML progress element for showing the
    amount of food. *)
let food_progres_bar = Html.getElementById "food"

(** Module wide value for the HTML progress element for showing the
    amount of electricity. *)
let electricity_progres_bar = Html.getElementById "electricity"

(** Module wide value for the HTML progress element for showing the
    amount of iron. *)
let iron_progres_bar = Html.getElementById "iron"

(** Module wide value for the HTML progress element for showing the
    amount of coal. *)
let coal_progres_bar = Html.getElementById "coal"

(** Module wide value for the HTML input element for saving a game state
    to JSON. *)
let save_button = get_element_by_id "save_button" Html.CoerceTo.button

(** Module wide value for the HTML div element for showing the selection
    of buildings available to be placed. *)
let building_selection = Html.getElementById "building_selection"

(** Module wide value for the HTML input element for submitting setup
    for a game session. *)
let submit = get_element_by_id "submit" Html.CoerceTo.input

(** Module wide value for the HTML label element for showing the
    currently selected cell size in game setup. *)
let label = get_element_by_id "cell_size_label" Html.CoerceTo.label

(** Module wide value for the HTML input element for selecting cell size
    in game setup. *)
let slider = get_element_by_id "cell_size" Html.CoerceTo.input

(** Module wide value for the HTML input element for uploading the
    current game save. *)
let game_save = get_element_by_id "game_save" Html.CoerceTo.input

(** List of texture names to be used in the GUI. *)
let texture_names =
  [
    "sand";
    "road";
    "house";
    "oats_plantation";
    "power_plant";
    "mine";
    "barrack";
  ]

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

(** [set_progress_bar state element name] changes the attributes of
    [element] to show some statistics about state. *)
let set_progress_bar (state : state) element name =
  element##setAttribute (Js.string "max") (Js.string "100");
  element##setAttribute (Js.string "value")
    (Js.string
       (List.find (fun (k, _) -> k = name) (stockpile state)
       |> snd |> string_of_int))

(** [set_money_progress_bar state] changes the attributes of the
    progress HTML element to show money. *)
let set_money_progress_bar (state : state) =
  set_progress_bar state money_progres_bar "money"

(** [set_food_progress_bar state] changes the attributes of the progress
    HTML element to show food. *)
let set_food_progress_bar (state : state) =
  set_progress_bar state food_progres_bar "oat"

(** [set_electricity_progress_bar state] changes the attributes of the
    progress HTML element to show food. *)
let set_electricity_progress_bar (state : state) =
  set_progress_bar state electricity_progres_bar "electricity"

(** [set_iron_progress_bar state] changes the attributes of the progress
    HTML element to show food. *)
let set_iron_progress_bar (state : state) =
  set_progress_bar state iron_progres_bar "iron"

(** [set_coal_progress_bar state] changes the attributes of the progress
    HTML element to show food. *)
let set_coal_progress_bar (state : state) =
  set_progress_bar state coal_progres_bar "coal"

(** [reset_canvas state] clears everything from the foreground canvas
    and the background canvas.*)
let reset_canvas (state : state) =
  bg_ctx##clearRect 0. 0.
    (state |> canvas_size |> fst |> float_of_int)
    (state |> canvas_size |> snd |> float_of_int);
  fg_ctx##clearRect 0. 0.
    (state |> canvas_size |> fst |> float_of_int)
    (state |> canvas_size |> snd |> float_of_int)

(** [setup_canvas state] setups the foreground canvas and the background
    canvas based on properties from [state]. *)
let setup_canvas (state : state) =
  bg_canvas##.width := state |> canvas_size |> fst;
  bg_canvas##.height := state |> canvas_size |> snd;
  fg_canvas##.width := state |> canvas_size |> fst;
  fg_canvas##.height := state |> canvas_size |> snd;
  bg_ctx##translate
    (float_of_int bg_canvas##.width /. 2.)
    ((state |> cell_size |> snd |> float_of_int) *. 2.);
  fg_ctx##translate
    (float_of_int fg_canvas##.width /. 2.)
    ((state |> cell_size |> snd |> float_of_int) *. 2.)

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
  let cell_width = state |> cell_size |> fst |> float_of_int in
  let cell_height = state |> cell_size |> snd |> float_of_int in
  bg_ctx##save;
  bg_ctx##translate
    ((float_of_int y -. float_of_int x) *. cell_width /. 2.)
    ((float_of_int x +. float_of_int y) *. cell_height /. 2.);
  bg_ctx##drawImage_full texture 0. 0. 130. 230. (-65.) 0. 130. 230.;
  bg_ctx##restore

(** [draw_map state] draws the cells in [state] on the background
    canvas.*)
let draw_map (state : state) =
  Array.iteri
    (fun i ->
      Array.iteri (fun j c ->
          let texture =
            match c with
            | Road _ -> find_texture "road"
            | Building b -> find_texture (building_name b)
            | None -> find_texture "sand"
          in
          draw_img state i j texture))
    (cells state)

(** [cell_positions state event] are the x and y indices of a cell in
    [state] that the mouse through [event] is currently hovering over. *)
let cell_positions state (event : Html.mouseEvent Js.t) =
  let canvas_width = state |> canvas_size |> fst |> float_of_int in
  let cell_width = state |> cell_size |> fst |> float_of_int in
  let cell_height = state |> cell_size |> snd |> float_of_int in
  let map_length = state |> map_length |> float_of_int in
  let padding_width =
    (canvas_width -. (cell_width *. map_length)) /. 2.
  in
  let mouse_x =
    float_of_int event##.clientX
    -. fg_canvas##getBoundingClientRect##.left
  in
  let mouse_y =
    float_of_int event##.clientY
    -. fg_canvas##getBoundingClientRect##.top
  in
  let x =
    ((mouse_x -. padding_width) /. cell_width) -. (map_length /. 2.)
  in
  let y = (mouse_y -. (cell_height *. 2.)) /. cell_height in
  (y -. x |> floor |> int_of_float, y +. x |> floor |> int_of_float)

(** [highlight state event] highlights a cell in [state] by calculating
    its positions with [event]. *)
let highlight (state : state) (event : Html.mouseEvent Js.t) =
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

let add_highlight_listener (state : state) =
  Html.addEventListener fg_canvas Html.Event.mousemove
    (Dom.handler (highlight state))
    Js._false
  |> ignore

let plot_cell (state : state) (event : Html.mouseEvent Js.t) =
  let positions = cell_positions state event in
  let a =
    if not (selected_building state) then "sand"
    else List.nth textures (current_selected state) |> fst
  in
  if selected_building state && a <> "road" && a <> "sand" then
    place_cell state
      (Building
         (List.find (fun b -> building_name b = a) (buildings state)))
      (fst positions) (snd positions);
  if selected_building state && a = "road" then
    place_cell state
      (Road (new_road 0 (fst positions) (snd positions)))
      (fst positions) (snd positions);
  if selected_building state && a = "sand" then
    place_cell state None (fst positions) (snd positions);
  draw_map state;
  Js._true

let add_plot_listener (state : state) =
  Html.addEventListener fg_canvas Html.Event.click
    (Dom.handler (plot_cell state))
    Js._false
  |> ignore

let draw_building_selections =
  List.mapi
    (fun i (n, t) ->
      let new_div = Html.createDiv Html.document in
      let label = Html.createLabel Html.document in
      label##.innerHTML := Js.string n;
      new_div##.style##.display := Js.string "block";
      t##.id := i |> string_of_int |> Js.string;
      Dom.appendChild building_selection new_div;
      Dom.appendChild new_div t;
      Dom.appendChild new_div label)
    textures
  |> ignore

let select state (event : Html.mouseEvent Js.t) =
  let event_target =
    Js.Opt.get event##.target (fun _ -> raise Not_found)
  in
  let event_id = Js.to_string event_target##.id in
  let current = Html.getElementById event_id in
  let previous =
    Html.getElementById_opt (string_of_int (current_selected state))
  in
  if match previous with None -> false | Some _ -> true then
    (match previous with
    | None -> failwith ""
    | Some element -> element)##.classList##remove
      (Js.string "selected");
  if
    selected_building state
    && event_id = (current_selected state |> string_of_int)
  then select_building state (-1)
  else select_building state (int_of_string event_id);
  if selected_building state then
    current##.classList##add (Js.string "selected");
  Js._true

let add_building_selection_listener (state : state) =
  List.mapi
    (fun i _ ->
      let img = Html.getElementById (string_of_int i) in
      Html.addEventListener img Html.Event.click
        (Dom.handler (select state))
        Js._false)
    textures
  |> ignore

let save_game (state : state) _ =
  let div = Html.getElementById "save" in
  let new_link = Html.createA Html.document in
  let encode_url =
    "data:text/json;charset=utf-8,"
    ^ Js.to_string
        (Js.encodeURIComponent (Js.string (save_state state)))
  in
  new_link##.href := Js.string encode_url;
  new_link##.innerHTML := Js.string "Click this link to download";
  Dom.appendChild div new_link;
  Js._true

let add_save_button_listener (state : state) =
  Html.addEventListener save_button Html.Event.click
    (Dom.handler (save_game state))
    Js._false
  |> ignore

let update_slider_label =
  let onchange_handler _ =
    let cell_size = Js.to_string slider##.value in
    label##.innerHTML := Js.string ("Cell Size: " ^ cell_size);
    Js._true
  in
  slider##.onchange := Dom.handler onchange_handler

let toggle_game is_hidden =
  let main_div = Html.getElementById "main" in
  if is_hidden then main_div##.classList##remove (Js.string "hidden")
  else main_div##.classList##add (Js.string "hidden")

let toggle_startup is_hidden =
  let startup = Html.getElementById "startup" in
  if is_hidden then startup##.classList##remove (Js.string "hidden")
  else startup##.classList##add (Js.string "hidden")

let add_event_listeners (state : state) =
  add_highlight_listener state;
  add_plot_listener state;
  add_building_selection_listener state;
  add_save_button_listener state

let draw_setup =
  update_slider_label;
  toggle_game false

let update_statistics (state : state) =
  set_money_progress_bar state;
  set_food_progress_bar state;
  set_electricity_progress_bar state;
  set_iron_progress_bar state;
  set_coal_progress_bar state

let setup_gui (state : state) =
  reset_canvas state;
  setup_canvas state;
  update_statistics state;
  draw_building_selections;
  add_event_listeners state;
  draw_map state

let rec game_loop (state : state) =
  update_statistics state;
  next_state state;
  Html.window##requestAnimationFrame
    (Js.wrap_callback (fun _ -> game_loop state))
  |> ignore

let handle_start_from_file _ =
  let file_lst =
    Js.Optdef.get game_save##.files (fun _ -> raise Not_found)
  in
  let file = Js.Opt.get (file_lst##item 0) (fun _ -> raise Not_found) in
  let file_reader = new%js File.fileReader in
  let file_load_handler e =
    let evt = Js.Opt.get e##.target (fun _ -> raise Not_found) in
    let result = evt##.result in
    let state =
      from_string
        (Js.Opt.get (File.CoerceTo.string result) (fun _ ->
             raise Not_found)
        |> Js.to_string)
    in
    toggle_startup false;
    toggle_game true;
    setup_gui state;
    game_loop state;
    Js._true
  in
  file_reader##.onload := Dom.handler file_load_handler;
  file_reader##readAsText file;
  Js._true

let handle_start_from_setup _ =
  let state =
    new_state "game_state.json" 1200 750
      (slider##.value |> Js.to_string |> int_of_string)
      128 64
  in
  toggle_startup false;
  toggle_game true;
  setup_gui state;
  game_loop state;
  Js._true

let start_game =
  game_save##.onchange := Dom.handler handle_start_from_file;
  submit##.onclick := Dom.handler handle_start_from_setup

let main =
  draw_setup;
  start_game
