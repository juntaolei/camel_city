open Cells
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

(** Module wide value for the HTML Div element with ID "main". *)
let main_div = Html.getElementById "main"

(** Module wide value for the HTML Div element with ID "startup". *)
let startup_div = Html.getElementById "startup"

(** Module wide value for the HTML Div element with ID "game". *)
let game_div = Html.getElementById "game"

(** Module wide value for the HTML Div element with ID "navbar_buttons". *)
let navbar_buttons_div = Html.getElementById "navbar_buttons"

(** Module wide value for the HTML Div element with ID "notification". *)
let notification_div = Html.getElementById "notification"

(** Module wide value for the HTML Div element with ID "info". *)
let info_div = Html.getElementById "info"

(** Module wide value for the HTML Div element with ID "info_container". *)
let info_container_div = Html.getElementById "info_container"

(** Module wide value for the HTML Div element with ID
    "notification_container". *)
let notification_container_div =
  Html.getElementById "notification_container"

(** Module wide value for the HTML div element for showing the selection
    of buildings available to be placed. *)
let selection_div = Html.getElementById "building_selection"

(** Module wide value for the HTML Span element for showing the amount
    of tick. *)
let tick_span = Html.getElementById "tick"

(** Module wide value for the HTML Span element for showing the amount
    of population. *)
let population_span = Html.getElementById "population"

(** Module wide value for the HTML Span element for showing the amount
    of money. *)
let money_span = Html.getElementById "money"

(** Module wide value for the HTML Span element for showing the amount
    of food. *)
let food_span = Html.getElementById "food"

(** Module wide value for the HTML Span element for showing the amount
    of electricity. *)
let electricity_span = Html.getElementById "electricity"

(** Module wide value for the HTML Span element for showing the amount
    of iron. *)
let iron_span = Html.getElementById "iron"

(** Module wide value for the HTML Span element for showing the amount
    of coal. *)
let coal_span = Html.getElementById "coal"

(** Module wide value for the HTML Span element for showing the deficit
    counter. *)
let deficit_span = Html.getElementById "deficit"

(** Module wide value for the HTML Span element for showing the
    starvation counter. *)
let starvation_span = Html.getElementById "starvation"

(** Module wide value for the HTML P element for showing any event
    notification. *)
let notification_content = Html.getElementById "notification_content"

(** Module wide value for the HTML Button element for saving a game
    state to JSON. *)
let save_button = get_element_by_id "save_button" Html.CoerceTo.button

(** Module wide value for the HTML Button element for pausing or
    unpausing the game. *)
let pause_button = get_element_by_id "pause_button" Html.CoerceTo.button

(** Module wide value for the HTML Button element for closing the event
    notification. *)
let notification_button =
  get_element_by_id "close_notificaation" Html.CoerceTo.button

(** Module wide value for the HTML Input element for submitting setup
    for a normal game session. *)
let submit = get_element_by_id "submit" Html.CoerceTo.input

(** Module wide value for the HTML Input element for submitting setup
    for a sandbox game session. *)
let sandbox = get_element_by_id "sandbox" Html.CoerceTo.input

(** Module wide value for the HTML Input element for selecting cell size
    in game setup. *)
let slider = get_element_by_id "cell_size" Html.CoerceTo.input

(** Module wide value for the HTML Input element for uploading the
    current game save. *)
let game_save = get_element_by_id "game_save" Html.CoerceTo.input

(** Module wide value for the HTML Input element for starting the
    current game save. *)
let start_save = get_element_by_id "start_save" Html.CoerceTo.input

(** Module wide value for the HTML Label element for showing the
    currently selected cell size in game setup. *)
let label = get_element_by_id "cell_size_label" Html.CoerceTo.label

(** List of texture names to be used in the GUI. *)
let texture_names =
  [
    "Sand";
    "Road";
    "House";
    "Oats Plantation";
    "Power Plant";
    "Mine";
    "Barrack";
    "Nice Apartment";
    "Steel Mill";
    "Canned Oats Factory";
    "Steel Market";
    "Iron Market";
    "Canned Oats Market";
    "The Wonder";
    "Coal Mine";
  ]

(** [create_img filename] is the image loaded based on a [filename]. *)
let create_img filename =
  let img = Html.createImg Html.document in
  img##.src := Js.string ("textures/" ^ filename ^ ".png");
  img

(** [textures] is the list of loaded textures to be used in the GUI. *)
let textures = List.map (fun x -> (x, create_img x)) texture_names

(** [find_texture name] is the loaded texture given by the texture
    [name]. *)
let find_texture name =
  List.find (fun x -> name = fst x) textures |> snd

(** [set_progress_bar state element name] changes the attributes of
    [element] to show some statistics about state. *)
let set_span state element name =
  let attribute_value =
    List.find (fun (k, _) -> k = name) state.stockpile |> snd
  in
  let string_value = attribute_value |> string_of_int in
  let inner_html =
    String.capitalize_ascii name ^ ": " ^ string_value |> Js.string
  in
  element##.innerHTML := inner_html

(** [set_tick_span state] changes the attributes of the HTML Span
    element to show tick. *)
let set_tick_span state =
  let string_value = state.tick |> string_of_int in
  let inner_html = "Tick: " ^ string_value |> Js.string in
  tick_span##.innerHTML := inner_html

(** [set_population_span state] changes the attributes of the HTML Span
    element to show tick. *)
let set_population_span state =
  let string_value = state.population |> string_of_int in
  let inner_html = "Population: " ^ string_value |> Js.string in
  population_span##.innerHTML := inner_html

(** [set_money_div state] changes the attributes of the HTML Span
    element to show money. *)
let set_money_span state = set_span state money_span "money"

(** [set_food_div state] changes the attributes of the HTML Span element
    to show food. *)
let set_food_span state = set_span state food_span "food"

(** [set_electricity_div state] changes the attributes of the HTML Span
    element to show food. *)
let set_electricity_span state =
  set_span state electricity_span "electricity"

(** [set_iron_div state] changes the attributes of the HTML Span element
    to show food. *)
let set_iron_span state = set_span state iron_span "iron"

(** [set_coal_div state] changes the attributes of the HTML Span element
    to show food. *)
let set_coal_span state = set_span state coal_span "coal"

(** [set_deficit_counter state] changes the attributes of the HTML Span
    element to the deficit counter. *)
let set_deficit_counter state =
  let string_value = state.deficit_counter |> string_of_int in
  let inner_html = "Deficit Counter: " ^ string_value |> Js.string in
  deficit_span##.innerHTML := inner_html

(** [set_starvation_counter state] changes the attributes of the HTML
    Span element to show the starvation counter. *)
let set_starvation_counter state =
  let string_value = state.starvation_counter |> string_of_int in
  let inner_html = "Starvation Counter: " ^ string_value |> Js.string in
  starvation_span##.innerHTML := inner_html

(** [set_pause_button state] changes the inner HTML of the pause button
    to show if the game is in paused or unpaused state. *)
let set_pause_button state =
  let string_value = if state.is_paused then "Resume" else "Pause" in
  let inner_html = string_value |> Js.string in
  pause_button##.innerHTML := inner_html

(** [reset_canvas state] clears everything from the foreground canvas
    and the background canvas.*)
let reset_canvas state =
  bg_ctx##clearRect 0. 0.
    (state.canvas_size |> fst |> float_of_int)
    (state.canvas_size |> snd |> float_of_int);
  fg_ctx##clearRect 0. 0.
    (state.canvas_size |> fst |> float_of_int)
    (state.canvas_size |> snd |> float_of_int)

(** [setup_canvas state] setups the foreground canvas and the background
    canvas based on properties from [state]. *)
let setup_canvas state =
  bg_canvas##.width := state.canvas_size |> fst;
  bg_canvas##.height := state.canvas_size |> snd;
  fg_canvas##.width := state.canvas_size |> fst;
  fg_canvas##.height := state.canvas_size |> snd;
  bg_ctx##translate
    (float_of_int bg_canvas##.width /. 2.)
    ((state.cell_size |> snd |> float_of_int) *. 2.);
  fg_ctx##translate
    (float_of_int fg_canvas##.width /. 2.)
    ((state.cell_size |> snd |> float_of_int) *. 2.)

(** [draw_cell state x y color] colors a cell of indices [x] and [y] in
    [state] with [color] on the foreground canvas. *)
let draw_cell state (x, y) color =
  let cell_width = state.cell_size |> fst in
  let cell_height = state.cell_size |> snd in
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
  let cell_width = state.cell_size |> fst |> float_of_int in
  let cell_height = state.cell_size |> snd |> float_of_int in
  bg_ctx##save;
  bg_ctx##translate
    ((float_of_int y -. float_of_int x) *. cell_width /. 2.)
    ((float_of_int x +. float_of_int y) *. cell_height /. 2.);
  bg_ctx##drawImage_full texture 0. 0. 130. 230. (-65.) 0. 130. 230.;
  bg_ctx##restore

(** [draw_map state] draws the cells in [state] on the background
    canvas.*)
let draw_map state =
  Array.iteri
    (fun i ->
      Array.iteri (fun j c ->
          let texture =
            match c with
            | Road _ -> find_texture "Road"
            | Building building -> find_texture building.name
            | None -> find_texture "Sand"
          in
          draw_img state i j texture))
    state.cells

(** [cell_positions state event] are the x and y indices of a cell in
    [state] that the mouse through [event] is currently hovering over. *)
let cell_positions state (event : Html.mouseEvent Js.t) =
  let canvas_width = state.canvas_size |> fst |> float_of_int in
  let cell_width = state.cell_size |> fst |> float_of_int in
  let cell_height = state.cell_size |> snd |> float_of_int in
  let map_length = state.map_length |> float_of_int in
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

(** [clean_info] clears the information section. *)
let clean_info =
  while info_div##hasChildNodes |> Js.to_bool do
    print_endline "yes";
    Dom.removeChild info_div
      (Js.Opt.get info_div##.firstChild (fun _ -> raise Not_found))
  done

(** [set_info state (x, y)] updates the text information in the info div
    when highlighting a cell in [state] with coordinates ([x], [y]). *)
let set_info state (x, y) =
  while info_div##hasChildNodes |> Js.to_bool do
    Dom.removeChild info_div
      (Js.Opt.get info_div##.firstChild (fun _ -> raise Not_found))
  done;
  let bold_name = Html.createDiv Html.document in
  bold_name##.className := Js.string "has-text-bold";
  match state.cells.(x).(y) with
  | None ->
      bold_name##.innerHTML := Js.string "Sand";
      Dom.appendChild info_div bold_name;
      Dom.appendChild info_container_div info_div
  | Road _ ->
      bold_name##.innerHTML := Js.string "Road";
      Dom.appendChild info_div bold_name;
      Dom.appendChild info_container_div info_div
  | Building building ->
      let output_name_div = Html.createDiv Html.document in
      let output_amount_div = Html.createDiv Html.document in
      let income_amount_div = Html.createDiv Html.document in
      let defense_amount_div = Html.createDiv Html.document in
      let maintenance_amount_div = Html.createDiv Html.document in
      let elements =
        [
          output_name_div;
          output_amount_div;
          income_amount_div;
          defense_amount_div;
          maintenance_amount_div;
        ]
      in
      List.iter
        (fun element -> element##.className := Js.string "is-size-7")
        elements;
      bold_name##.innerHTML :=
        building.name |> String.capitalize_ascii |> Js.string;
      output_name_div##.innerHTML
      := building.output |> resource_name
         |> ( ^ ) "Output Resource: "
         |> Js.string;
      output_amount_div##.innerHTML
      := building.output |> resource_amount |> string_of_int
         |> ( ^ ) "Output Amount: " |> Js.string;
      income_amount_div##.innerHTML
      := building.income |> string_of_int |> ( ^ ) "Income: "
         |> Js.string;
      defense_amount_div##.innerHTML
      := building.defense |> string_of_int |> ( ^ ) "Defense: "
         |> Js.string;
      maintenance_amount_div##.innerHTML
      := building.maintenance |> string_of_int |> ( ^ ) "Maintenance: "
         |> Js.string;
      Dom.appendChild info_div bold_name;
      if building.output |> resource_name <> "" then begin
        Dom.appendChild info_div output_name_div;
        Dom.appendChild info_div output_amount_div
      end;
      Dom.appendChild info_div income_amount_div;
      Dom.appendChild info_div defense_amount_div;
      Dom.appendChild info_div maintenance_amount_div;
      Dom.appendChild info_container_div info_div

(** [highlight state event] highlights a cell in [state] by calculating
    its positions with [event]. *)
let highlight state (event : Html.mouseEvent Js.t) =
  let canvas_width = state.canvas_size |> fst in
  let canvas_height = state.canvas_size |> snd in
  let map_length = state.map_length in
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
  then begin
    draw_cell state positions "hsla(60, 100%, 50%, 0.25)";
    set_info state positions
  end
  else clean_info;
  Js._true

(** [add_highlight_listener state] adds an event listener to the
    foreground canvas that highlights a cell in [state] when hovered
    over.*)
let add_highlight_listener state =
  Html.addEventListener fg_canvas Html.Event.mousemove
    (Dom.handler (highlight state))
    Js._false
  |> ignore

(** [plot_cell state event] plots a cell on [state] when a cell's event
    handler is triggered by a click [event]. *)
let plot_cell state (event : Html.mouseEvent Js.t) =
  let positions = cell_positions state event in
  let name =
    if not (is_selected state) then "sand"
    else List.nth textures state.selected_cell |> fst
  in
  if is_selected state && name <> "road" && name <> "sand" then
    place_building state name (fst positions) (snd positions);
  if is_selected state && name = "road" then
    place_cell state
      (Road (new_road (fst positions) (snd positions)))
      (fst positions) (snd positions);
  if is_selected state && name = "sand" then
    place_cell state None (fst positions) (snd positions);
  draw_map state;
  Js._true

(** [add_plot_listener state] adds an event listener to the foreground
    canvas that reacts to a click event to plot a cell in [state]. *)
let add_plot_listener state =
  Html.addEventListener fg_canvas Html.Event.click
    (Dom.handler (plot_cell state))
    Js._false
  |> ignore

(** [find_cost lst name] is the cost of [name] as specified in [lst]. *)
let find_cost lst name =
  try (List.find (fun b -> b.name = name) lst).cost |> string_of_int
  with Not_found -> "0"

(** [draw_selections] draw the list of available buildings that can be
    plotted on the GUI. *)
let draw_selections lst =
  List.mapi
    (fun i (name, img) ->
      let box = Html.createDiv Html.document in
      let span = Html.createSpan Html.document in
      span##.innerHTML := Js.string (name ^ " $" ^ find_cost lst name);
      span##.className := Js.string "is-size-7 has-text-weight-light";
      span##.id := i |> string_of_int |> Js.string;
      box##.className :=
        Js.string
          "block mr-3 my-3 p-2 is-flex is-flex-direction-column \
           is-justify-content-space-between is-align-items-center";
      box##.style##.minWidth := Js.string "100px";
      box##.id := i |> string_of_int |> Js.string;
      box##.style##.border := Js.string "1px solid hsl(0, 0%, 96%)";
      box##.style##.borderRadius := Js.string "5px";
      img##.id := i |> string_of_int |> Js.string;
      img##.className := Js.string "image is-64x64";
      Dom.appendChild selection_div box;
      Dom.appendChild box img;
      Dom.appendChild box span)
    textures
  |> ignore

(** [select state event] highlights and select from a list of buildings
    available in [state] by listening to the click [event]. *)
let select state (event : Html.mouseEvent Js.t) =
  let event_target =
    Js.Opt.get event##.target (fun _ -> raise Not_found)
  in
  let event_id = Js.to_string event_target##.id in
  let current = Html.getElementById event_id in
  let previous =
    Html.getElementById_opt (string_of_int state.selected_cell)
  in
  if match previous with None -> false | Some _ -> true then
    (match previous with
    | None -> failwith ""
    | Some element -> element)##.classList##remove
      (Js.string "selected");
  if
    is_selected state
    && event_id = (state.selected_cell |> string_of_int)
  then select_cell state (-1)
  else select_cell state (int_of_string event_id);
  if is_selected state then
    current##.classList##add (Js.string "selected");
  Js._true

(** [add_building_selection_listener state] adds the event listener that
    reacts to a building from the building selection pane being
    selected. *)
let add_building_selection_listener state =
  List.mapi
    (fun i _ ->
      let img = Html.getElementById (string_of_int i) in
      Html.addEventListener img Html.Event.click
        (Dom.handler (select state))
        Js._false)
    textures
  |> ignore

(** [save_game state _] converts the current game state to a JSON file. *)
let save_game state _ =
  let modal = Html.createDiv Html.document in
  let modal_background = Html.createDiv Html.document in
  let modal_content = Html.createDiv Html.document in
  let modal_button = Html.createButton Html.document in
  let new_link = Html.createA Html.document in
  let encode_url =
    state |> save_state |> Js.string |> Js.encodeURIComponent
    |> Js.to_string
    |> ( ^ ) "data:text/json;charset=utf-8,"
  in
  modal##.className := Js.string "modal is-active";
  modal_background##.className := Js.string "modal-background";
  modal_content##.className
  := Js.string "modal-content is-flex is-justify-content-center";
  modal_button##.className := Js.string "modal-close is-large";
  Html.addEventListener modal_button Html.Event.click
    (Dom.handler (fun _ ->
         Dom.removeChild main_div modal;
         Js._true))
    Js._false
  |> ignore;
  new_link##.id := Js.string "download";
  new_link##.href := Js.string encode_url;
  new_link##.innerHTML := Js.string "Download";
  new_link##.className := Js.string "button is-success";
  new_link##setAttribute (Js.string "download")
    (Js.string "game_save.json");
  Dom.appendChild modal modal_background;
  Dom.appendChild modal modal_content;
  Dom.appendChild modal modal_button;
  Dom.appendChild modal_content new_link;
  Dom.appendChild main_div modal;
  Js._true

(** [add_save_button_listener state] adds an event listener to the save
    game button when being clicked. *)
let add_save_button_listener state =
  Html.addEventListener save_button Html.Event.click
    (Dom.handler (save_game state))
    Js._false
  |> ignore

(** [add_pause_button_listener state] adds an event listener to the
    pause game button when being clicked. *)
let add_pause_button_listener state =
  Html.addEventListener pause_button Html.Event.click
    (Dom.handler (fun _ ->
         state.is_paused <- not state.is_paused;
         Js._true))
    Js._false
  |> ignore

(** [delete_notification] removes the HTML elements related to showing
    the event notification. *)
let delete_notification =
  Dom.removeChild notification_div notification_button;
  Dom.removeChild notification_div notification_content;
  Dom.removeChild notification_container_div notification_div

(** [draw_notification state] draws the HTML elements related to event
    notification. *)
let draw_notification state =
  let notification_handler _ =
    if state.text <> "" then begin
      Dom.removeChild notification_div notification_button;
      Dom.removeChild notification_div notification_content;
      Dom.removeChild notification_container_div notification_div
    end;
    state.text <- "";
    Js._true
  in
  notification_content##.innerHTML := Js.string state.text;
  Dom.appendChild notification_div notification_button;
  Dom.appendChild notification_div notification_content;
  Dom.appendChild notification_container_div notification_div;
  Html.addEventListener notification_button Html.Event.click
    (Dom.handler notification_handler)
    Js._false
  |> ignore

(** [update_slider_label] changes the label value for the game cell size
    selection slider. *)
let update_slider_label =
  let onchange_handler _ =
    let cell_size = Js.to_string slider##.value in
    label##.innerHTML := Js.string ("Cell Size: " ^ cell_size);
    Js._true
  in
  slider##.onchange := Dom.handler onchange_handler

(** [toggle_game is_shown] hides the game session based on if it
    [is_shown]. *)
let toggle_game is_shown =
  if is_shown then begin
    Dom.appendChild navbar_buttons_div pause_button;
    Dom.appendChild navbar_buttons_div save_button;
    Dom.appendChild main_div game_div
  end
  else begin
    Dom.removeChild navbar_buttons_div pause_button;
    Dom.removeChild navbar_buttons_div save_button;
    Dom.removeChild main_div game_div
  end

(** [toggle_startup is_shown] hides the game setup screen based on if it
    [is_shown]. *)
let toggle_startup is_shown =
  if is_shown then Dom.appendChild main_div startup_div
  else Dom.removeChild main_div startup_div

(** [add_event_listeners state] registers all event listeners for the
    GUI. *)
let add_event_listeners state =
  add_highlight_listener state;
  add_plot_listener state;
  add_building_selection_listener state;
  add_save_button_listener state;
  add_pause_button_listener state

(** [draw_setup] creates the initial game setup screen. *)
let draw_setup =
  update_slider_label;
  toggle_startup true;
  toggle_game false

(** [update_statistics state] updates the various progress bars in the
    game GUI. *)
let update_statistics state =
  set_pause_button state;
  set_tick_span state;
  set_population_span state;
  set_money_span state;
  set_food_span state;
  set_electricity_span state;
  set_iron_span state;
  set_coal_span state;
  set_deficit_counter state;
  set_starvation_counter state;
  if String.length state.text <> 0 then draw_map state

(** [setup_gui state] setups the game session based on the [state]. *)
let setup_gui state =
  reset_canvas state;
  setup_canvas state;
  update_statistics state;
  draw_selections state.buildings;
  add_event_listeners state;
  draw_map state

(** [draw_game_over state] draws the game over screen for the user when
    the game is over according to [state]. *)
let draw_game_over state =
  let modal = Html.createDiv Html.document in
  let modal_background = Html.createDiv Html.document in
  let modal_content = Html.createDiv Html.document in
  let modal_button = Html.createButton Html.document in
  let message = Html.createH1 Html.document in
  modal##.className := Js.string "modal is-active";
  modal_background##.className := Js.string "modal-background";
  modal_content##.className
  := Js.string
       "modal-content is-flex is-justify-content-center has-text-light";
  modal_button##.className := Js.string "modal-close is-large";
  Html.addEventListener modal_button Html.Event.click
    (Dom.handler (fun _ ->
         Html.window##.location##reload;
         Js._true))
    Js._false
  |> ignore;
  message##.innerHTML := Js.string state.game_over_message;
  Dom.appendChild modal modal_background;
  Dom.appendChild modal modal_content;
  Dom.appendChild modal modal_button;
  Dom.appendChild modal_content message;
  Dom.appendChild main_div modal

(** [game_loop state] is the main recursive game loop in which parts of
    the GUI is updated when the [state] is updated. *)
let rec game_loop state =
  let next_state_timer state =
    let update_time = state.last_updated +. state.interval_time in
    let current_time = Unix.gettimeofday () in
    let is_next_state = current_time > update_time in
    if is_next_state then next_state state;
    if is_next_state then state.last_updated <- current_time
  in
  if not state.is_game_over then begin
    update_statistics state;
    next_state_timer state;
    if state.text <> "" then draw_notification state;
    Html.window##requestAnimationFrame
      (Js.wrap_callback (fun _ -> game_loop state))
    |> ignore
  end
  else draw_game_over state

let trigger_game_loop state =
  toggle_startup false;
  toggle_game true;
  setup_gui state;
  delete_notification;
  game_loop state

(** [handle_start_from_file _] starts the game session from an user
    provided save file. *)
let handle_start_from_file _ =
  let opt_get opt_value =
    Js.Opt.get opt_value (fun _ -> raise Not_found)
  in
  let file_lst =
    Js.Optdef.get game_save##.files (fun _ -> raise Not_found)
  in
  if file_lst##.length > 0 then begin
    let file = opt_get (file_lst##item 0) in
    let file_reader = new%js File.fileReader in
    let file_load_handler event =
      let event_target = opt_get event##.target in
      event_target##.result |> File.CoerceTo.string |> opt_get
      |> Js.to_string |> from_string |> trigger_game_loop;
      Js._true
    in
    file_reader##.onload := Dom.handler file_load_handler;
    file_reader##readAsText file
  end;
  Js._true

(** [handle_start_from_setup _] starts the game session from the user
    provided settings. *)
let handle_start_from_setup _ =
  new_state 1200 750
    (slider##.value |> Js.to_string |> int_of_string)
    128 64
  |> trigger_game_loop;
  Js._true

(** [handle_start_from_setup_sandbox _] starts the game session in
    sandbox mode from the user provided settings. *)
let handle_start_from_setup_sandbox _ =
  let state =
    new_state ~is_sandbox:true 1200 750
      (slider##.value |> Js.to_string |> int_of_string)
      128 64
  in
  state.stockpile <-
    ("money", 1000000)
    :: List.filter (fun (k, _) -> k <> "money") state.stockpile;
  state |> trigger_game_loop;
  Js._true

(** [start_game] registers the two main event handler that starts the
    game. *)
let start_game =
  start_save##.onclick := Dom.handler handle_start_from_file;
  submit##.onclick := Dom.handler handle_start_from_setup;
  sandbox##.onclick := Dom.handler handle_start_from_setup_sandbox

let main =
  draw_setup;
  start_game
