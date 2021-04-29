open Buildings
open Yojson.Basic.Util
open Js_of_ocaml

type stockpile = resource list

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

let current_selected state = state.selected_building

let select_building state i = state.selected_building <- i

let selected_building state = state.selected_building >= 0

(** [build_cell_lst width height] is an two dimensional array with
    [width] and [height]. *)
let build_cell_lst width height = Array.make_matrix width height None

(** [place_cell state cell x y] updates the old cell as indexed by [x]
    and [y] in [state] with a new [cell]. *)
let place_cell state cell x y = state.cells.(x).(y) <- cell

let canvas_size state = state.canvas_size

let map_length state = state.map_length

let cell_size state = state.cell_size

let cells state = state.cells

let buildings state = state.buildings

let str_of_cell = function
  | None -> ""
  | Road _ -> "road"
  | Building b -> building_name b

let tick state = state.tick

let population state = state.population

let stockpile state = state.stockpile

let get_buildings state = state.buildings

let filename state = state.filename

let get_index id lst = List.nth lst id

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

let is_stockpile_sufficient building (stockpile : stockpile) : bool =
  resource_dependency building
  |> List.fold_left
       (fun acc dependency ->
         let name = resource_name dependency in
         let amount = resource_amount dependency in
         let new_amount = List.assoc name stockpile - amount in
         if new_amount >= 0 then acc && true else acc && false)
       true

(** [subtract_maintenace building stockpile] aubtracts the money that
    the building generates to it's value in resource stockpile*)
let subtract_maintenace building (stockpile : stockpile) : stockpile =
  let new_money = List.assoc "money" stockpile - maintenance building in
  List.map
    (fun (name, value) ->
      if name = "money" then (name, new_money) else (name, value))
    stockpile

let subtract_resource building (stockpile : stockpile) : stockpile =
  let output = output building in
  let output_name = resource_name output in
  let building_name = resource_name output in
  let change = resource_amount output in
  if building_name = "" then stockpile
  else
    List.map
      (fun (name, value) ->
        if name = output_name then (name, value - change)
        else (name, value))
      stockpile

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
  let building_name = resource_name output in
  let change = resource_amount output in
  if building_name = "" then stockpile
  else
    List.map
      (fun (name, value) ->
        if name = output_name then (name, value + change)
        else (name, value))
      stockpile

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
            |> add_income h |> add_resource h
          in
          update_stockpile_aux new_stockpile t
  in
  update_stockpile_aux stockpile lst

let available_buildings (state : state) =
  Array.fold_left
    (fun acc row ->
      Array.fold_left
        (fun acc cell ->
          match cell with Building b -> b :: acc | _ -> acc)
        [] row
      :: acc)
    [] state.cells
  |> List.flatten

let next_state (state : state) =
  state.stockpile <-
    update_stockpile (available_buildings state) state.stockpile;
  state.tick <- state.tick + 1
(* new_state ~stockpile:new_stockpile ~tick:(state.tick + 1) (filename
   state) (canvas_size state |> fst) (canvas_size state |> snd)
   (map_length state) (cell_size state |> fst) (cell_size state |> snd) *)

(** [init_stockpile acc json] is the list of initialized [stockpile]
    extracted from the json object [json]. *)
let init_stockpile json =
  List.fold_left
    (fun acc resource ->
      new_resource
        (resource |> member "name" |> to_string)
        (resource |> member "amount" |> to_string |> int_of_string)
      :: acc)
    [] json

(** [init_new_building name bld_lst] is a new building with name [name]
    if such building exists in [blk_lst]. Otherwise a failure is raised. *)
let init_new_building name lst =
  List.find (fun building -> building_name building = name) lst

(** [iter_cells s acc json] is the updated cells from information in
    [json]. *)
let iter_cells state cells json =
  let x cell = cell |> member "x" |> to_string |> int_of_string in
  let y cell = cell |> member "y" |> to_string |> int_of_string in
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
        ("amount", `String (resource_amount resource |> string_of_int));
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
        ("x", `String (string_of_int i));
        ("y", `String (string_of_int j));
        ("object", `String (str_of_cell cell));
      ]
  in
  Array.fold_left (fun acc row -> Array.to_list row :: acc) [] cells
  |> List.mapi (fun i row ->
         List.mapi (fun j cell -> json_of_cell i j cell) row)
  |> List.flatten

let from_string string =
  let json = Yojson.Basic.from_string string in
  let get_field name coord =
    json |> member name |> member coord |> to_string |> int_of_string
  in
  let init_state =
    new_state string
      (get_field "canvas_size" "x")
      (get_field "canvas_size" "y")
      (json |> member "map_length" |> to_string |> int_of_string)
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
    population =
      json |> member "population" |> to_string |> int_of_string;
  }

let save_state s =
  `Assoc
    [
      ("tick", `String (string_of_int (tick s)));
      ( "canvas_size",
        `Assoc
          [
            ("x", `String (string_of_int (canvas_size s |> fst)));
            ("y", `String (string_of_int (canvas_size s |> snd)));
          ] );
      ("map_length", `String (string_of_int (map_length s)));
      ( "cell_size",
        `Assoc
          [
            ("x", `String (string_of_int (cell_size s |> fst)));
            ("y", `String (string_of_int (cell_size s |> snd)));
          ] );
      ("cells", `List (generate_cell_lst s.cells));
      ("population", `String (string_of_int (population s)));
      ("stockpile", `List (generate_stockpile_lst [] s.stockpile));
    ]
  |> Yojson.to_string
