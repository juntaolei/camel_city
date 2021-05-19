open Buildings
open Yojson.Basic.Util

type stockpile = resource list

(** [default_stockpile] is the stockpile on any fresh game sessions. *)
let default_stockpile : stockpile =
  [
    ("money", 0);
    ("electricity", 0);
    ("oat", 0);
    ("iron", 0);
    ("coal", 0);
  ]

type cell =
  | Building of building
  | Road of road
  | None

type state = {
  mutable text : string;
  mutable filename : string;
  mutable tick : int;
  mutable canvas_size : int * int;
  mutable map_length : int;
  mutable cell_size : int * int;
  mutable population : int;
  mutable stockpile : stockpile;
  mutable buildings : building list;
  mutable cells : cell array array;
  mutable selected_building : int;
  mutable pause : bool;
}

(** [build_cell_lst width height] is an two dimensional array with
    [width] and [height]. *)
let build_cell_lst width height = Array.make_matrix width height None

let current_selected state = state.selected_building

let select_building state i = state.selected_building <- i

let selected_building state = state.selected_building >= 0

let place_cell state cell x y = state.cells.(x).(y) <- cell

let canvas_size state = state.canvas_size

let map_length state = state.map_length

let cell_size state = state.cell_size

let cells state = state.cells

let buildings state = state.buildings

let tick state = state.tick

let population state = state.population

let stockpile state = state.stockpile

let text state = state.text

let str_of_cell = function
  | None -> ""
  | Road _ -> "road"
  | Building b -> building_name b

(** [iter_buildings acc json] is the list of initialized [buildings]
    extracted from the json object [json]. *)
let iter_buildings json =
  List.fold_left
    (fun acc building ->
      let name = building |> member "name" |> to_string in
      let cost = building |> member "cost" |> to_int in
      let maintenance = building |> member "maintenance" |> to_int in
      let output =
        try
          let output_amount =
            building |> member "output" |> member "amount" |> to_int
          in
          let output_name =
            building |> member "output" |> member "name" |> to_string
          in
          (output_name, output_amount)
        with _ -> ("", 0)
      in
      let tax = building |> member "tax" |> to_int in
      let defense = building |> member "defense" |> to_int in
      let resource_dependency =
        building
        |> member "resource_dependency"
        |> to_list
        |> List.map (fun pair ->
               ( pair |> member "name" |> to_string,
                 pair |> member "amount" |> to_int ))
      in
      new_building name cost maintenance output tax defense
        resource_dependency
      :: acc)
    [] json

(** [buildings_init] is a list of buildings initialized from a
    configuration building list JSON file. *)
let buildings_init =
  "buildings_init.json" |> Yojson.Basic.from_file |> member "buildings"
  |> to_list |> iter_buildings

let new_state
    ?(stockpile = default_stockpile)
    ?(tick = 1)
    (filename : string)
    (canvas_width : int)
    (canvas_height : int)
    (map_length : int)
    (cell_width : int)
    (cell_height : int) =
  {
    tick;
    filename;
    text = "";
    canvas_size = (canvas_width, canvas_height);
    map_length;
    cell_size = (cell_width, cell_height);
    stockpile;
    buildings = buildings_init;
    population = 0;
    cells = build_cell_lst map_length map_length;
    selected_building = -1;
    pause = false;
  }

(** [is_stockpile_sufficient building stockiple] will check if the
    requirements for a building will make the values of [stockpile] to
    be negative. *)
let is_stockpile_sufficient building (stockpile : stockpile) : bool =
  resource_dependency building
  |> List.fold_left
       (fun acc dependency ->
         let name = resource_name dependency in
         let amount = resource_amount dependency in
         let new_amount = List.assoc name stockpile - amount in
         if new_amount >= 0 then acc && true else acc && false)
       true

(** [subtract_maintenace building stockpile] subtracts the money that
    the building generates to it's value in resource stockpile*)
let subtract_maintenace building (stockpile : stockpile) : stockpile =
  let new_money = List.assoc "money" stockpile - maintenance building in
  let filter_money (name, value) =
    if name = "money" then (name, new_money) else (name, value)
  in
  List.map filter_money stockpile

(** [subtract_dependency building stockpile] subtracts the required
    resources that is needed by [building] from the [stockpile]. *)
let subtract_dependency building (stockpile : stockpile) : stockpile =
  let resource_dependency = resource_dependency building in
  let rec subtract_dependency_aux resource_dependency stockpile =
    match resource_dependency with
    | [] -> stockpile
    | h :: t ->
        let output_name = resource_name h in
        let change = resource_amount h in
        let filter_value (name, value) =
          if name = output_name then (name, value - change)
          else (name, value)
        in
        if output_name = "" then stockpile
        else subtract_dependency_aux t (List.map filter_value stockpile)
  in
  subtract_dependency_aux resource_dependency stockpile

(** [add_income] adds the money that the building generates to it's
    value in resource stockpile*)
let add_income building (stockpile : stockpile) : stockpile =
  let new_money = List.assoc "money" stockpile + income building in
  List.map
    (fun (name, value) ->
      if name = "money" then (name, new_money) else (name, value))
    stockpile

(** [add_resources] adds the resource that the building generates to
    it's value in resource stockpile*)
let add_resource building (stockpile : stockpile) : stockpile =
  let output = output building in
  let output_name = resource_name output in
  let change = resource_amount output in
  if output_name = "" then stockpile
  else
    List.map
      (fun (name, value) ->
        if name = output_name then (name, value + change)
        else (name, value))
      stockpile

(** [update_stockpile lst stockpile] updates the current [stockpile]
    after accounting for the output and dependencies of the buildings in
    [lst]. *)
let update_stockpile lst stockpile =
  let rec update_stockpile_aux stockpile = function
    | [] -> stockpile
    | h :: t ->
        if not (is_stockpile_sufficient h stockpile) then
          let new_stockpile = subtract_maintenace h stockpile in
          update_stockpile_aux new_stockpile t
        else
          let new_stockpile =
            subtract_maintenace h stockpile
            |> subtract_dependency h |> add_income h |> add_resource h
          in
          update_stockpile_aux new_stockpile t
  in
  update_stockpile_aux stockpile lst

(** [available_buildings state] is the list of buildings plotted inside
    the cells in [state]. *)
let available_buildings state =
  Array.fold_left
    (fun acc row ->
      Array.fold_left
        (fun acc cell ->
          match cell with Building b -> b :: acc | _ -> acc)
        [] row
      :: acc)
    [] state.cells
  |> List.flatten

(** [init_stockpile acc json] is the list of initialized [stockpile]
    extracted from the json object [json]. *)
let init_stockpile json =
  List.fold_left
    (fun acc resource ->
      new_resource
        (resource |> member "name" |> to_string)
        (resource |> member "amount" |> to_int)
      :: acc)
    [] json

(** [init_new_building name bld_lst] is a new building with name [name]
    if such building exists in [blk_lst]. Otherwise a failure is raised. *)
let init_new_building name lst =
  List.find (fun building -> building_name building = name) lst

(** [iter_cells s acc json] is the updated cells from information in
    [json]. *)
let iter_cells state cells json =
  let x cell = cell |> member "x" |> to_int in
  let y cell = cell |> member "y" |> to_int in
  let obj cell = cell |> member "object" |> to_string in
  List.iter
    (fun cell ->
      if obj cell <> "" then
        if obj cell = "road" then
          cells.(x cell).(y cell) <- Road (new_road 0 (x cell) (y cell))
        else
          cells.(x cell).(y cell) <-
            Building (init_new_building (obj cell) state.buildings))
    json;
  cells

(** [generate_stock_lst acc pile] is the `List containing contents of
    [pile]. *)
let generate_stockpile_lst stockpile =
  let json_of_resource resource =
    `Assoc
      [
        ("name", `String (resource_name resource));
        ("amount", `Int (resource_amount resource));
      ]
  in
  List.fold_left
    (fun acc resource -> json_of_resource resource :: acc)
    stockpile

(** [generate_cell_lst cells] is the `List containing contents of
    [pile]. *)
let generate_cell_lst cells =
  let str_of_cell = function
    | None -> ""
    | Building t -> building_name t
    | Road _ -> "road"
  in
  let json_of_cell i j cell =
    `Assoc
      [
        ("x", `Int i);
        ("y", `Int j);
        ("object", `String (str_of_cell cell));
      ]
  in
  Array.fold_right (fun row acc -> Array.to_list row :: acc) cells []
  |> List.mapi (fun i row ->
         List.mapi (fun j cell -> json_of_cell i j cell) row)
  |> List.flatten

let from_string string =
  let json = Yojson.Basic.from_string string in
  let get_field name coordinate =
    json |> member name |> member coordinate |> to_int
  in
  let init_state =
    new_state string
      (get_field "canvas_size" "x")
      (get_field "canvas_size" "y")
      (json |> member "map_length" |> to_int)
      (get_field "cell_size" "x")
      (get_field "cell_size" "y")
  in
  {
    init_state with
    cells =
      iter_cells init_state init_state.cells
        (json |> member "cells" |> to_list);
    stockpile =
      List.rev (init_stockpile (json |> member "stockpile" |> to_list));
    population = json |> member "population" |> to_int;
  }

let save_state s =
  `Assoc
    [
      ("tick", `String (string_of_int (tick s)));
      ( "canvas_size",
        `Assoc
          [
            ("x", `Int (canvas_size s |> fst));
            ("y", `Int (canvas_size s |> snd));
          ] );
      ("map_length", `Int (map_length s));
      ( "cell_size",
        `Assoc
          [
            ("x", `Int (cell_size s |> fst));
            ("y", `Int (cell_size s |> snd));
          ] );
      ("cells", `List (generate_cell_lst s.cells));
      ("population", `Int (population s));
      ("stockpile", `List (generate_stockpile_lst [] s.stockpile));
    ]
  |> Yojson.to_string

(** [iter_resources lst] is the resource list extracted from the json list 
    [lst]. *)
let iter_resources lst = 
  List.fold_left
  (fun acc res ->
    let name = res |> member "name" |> to_string in
    let amount = res |> member "amount" |> to_int in
    let new_res = new_resource name amount in
    new_res :: acc)
  [] lst

(** [iter_events json] is the [(string * resource list) list] extracted from 
    the json list [json]. *)
let iter_events json =
  List.fold_left
  (fun acc event ->
    let text = event |> member "text" |> to_string in
    let resource_lst = event |> member "resource" |> to_list |> iter_resources
    in
    let damage = event |> member "defense" |> to_int in
    (text, resource_lst, damage)
    :: acc)
  [] json

(** [load_events] extracts the lists of easy, medium and hard events from the
    json file provided. *)
let load_events =
  let json_e = "events_init.json" |> Yojson.Basic.from_file |> member "easy"
  |> to_list |> iter_events in
  let json_m = "events_init.json" |> Yojson.Basic.from_file |> member "medium"
  |> to_list |> iter_events in
  let json_h = "events_init.json" |> Yojson.Basic.from_file |> member "hard"
  |> to_list |> iter_events in
  [("easy", json_e);("medium", json_m);("hard", json_h)]

(** [merge_stock pile stockpile] is the updated [stockpile] after adding or
    subtracting resources from [pile]. *)
let rec merge_stock pile = function
| [] -> pile
| h :: t ->  
  let r_name = resource_name h in
  let r_amount = resource_amount h in
  let filter_resource (name, value) =
    if name = r_name then (name, value + r_amount) else (name, value)
  in
  merge_stock (List.map filter_resource pile) t

(** [update_cells arr def] updates arr by decreasing defense levels of all 
    building by def and removes buildings with zero defense. *)
  let update_cells arr def =
  for i = 0 to Array.length arr do
    for j = 0 to Array.length arr.(0) do
      match arr.(i).(j) with
      | Building b -> begin
        let curr = defense b in
        if curr > def then
          arr.(i).(j) <- Building (dec_defense b def)
        else arr.(i).(j) <- None
      end
      | _ -> ()
    done
  done

let generate_event st = 
  let events = load_events in
  let level = List.assoc "money" (stockpile st) in
  let str = begin
    if level < 300 then "easy"
    else if level < 1000 then "medium"
    else "hard"
  end in
  let lst_e = List.assoc str events in
  let (f, s, t) = List.nth lst_e (Random.int (List.length lst_e)) in
  (f, merge_stock st.stockpile s, t)
  
let next_state (state : state) =
  if state.tick mod 60 = 0 then 
    let new_state = 
      update_stockpile (available_buildings state) state.stockpile in
    state.stockpile <- new_state;
    if state.tick mod 2400 = 0 then
      let (t, s, c) = generate_event state in
        state.text <- t;
        state.stockpile <- s;
        update_cells state.cells c;
  state.tick <- state.tick + 1