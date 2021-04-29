open Buildings
open Yojson.Basic.Util

type stockpile = resource list

(* this is unused? *)
let resource_stockpile : stockpile =
  [
    Buildings.new_resource "money" 0;
    ("electricity", 0);
    ("oat", 0);
    ("iron", 0);
    ("coal", 0);
  ]

let pause = false

type cell =
  | Building of building
  | Road of road
  | None

type state = {
  filename : string;
  tick : int;
  canvas_size : int * int;
  map_length : int;
  cell_size : int * int;
  mutable cells : cell array array;
  population : int;
  stockpile : stockpile;
  buildings : building list;
  mutable selected_building : int;
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
      new_building
        (building |> member "name" |> to_string)
        (building |> member "cost" |> to_string |> int_of_string)
        (building |> member "maintenance" |> to_string |> int_of_string)
        (building |> member "output" |> member "amount" |> to_string
       |> int_of_string)
        (building |> member "output" |> member "name" |> to_string)
        (building |> member "tax" |> to_string |> int_of_string)
        (building |> member "defense" |> to_string |> int_of_string)
        (building
        |> member "resource_dependency"
        |> to_list |> get_index 0 |> member "amount" |> to_string
        |> int_of_string)
        (building
        |> member "resource_dependency"
        |> to_list |> get_index 0 |> member "name" |> to_string)
      :: acc)
    [] json

let buildings_init =
  "buildings_init.json" |> Yojson.Basic.from_file |> member "buildings"
  |> to_list |> iter_buildings

let new_state
    ?(stockpile = resource_stockpile)
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
    cells = build_cell_lst map_length map_length;
    stockpile;
    buildings = buildings_init;
    population = 0;
    selected_building = -1;
  }

let rec resource_sufficiency_check_helper
    dependencies
    (stockpile : stockpile) =
  match dependencies with
  | [] -> true
  | h :: t ->
      let resource_name = Buildings.resource_name h in
      let resource_change = Buildings.resource_amount h in
      let new_resource_value =
        (* MAJOR PROBLEM: Throws not found error. *)
        List.assoc resource_name stockpile - resource_change
      in
      if new_resource_value >= 0 then
        true && resource_sufficiency_check_helper t stockpile
      else false && resource_sufficiency_check_helper t stockpile

let resource_sufficiency_check building stockpile : bool =
  let dependencies = Buildings.resource_dependency building in
  resource_sufficiency_check_helper dependencies stockpile

(** [sub_resource] subtracts the resources that the building uses from
    their values in resource stockpile*)
let rec sub_resource_helper dependencies stockpile : stockpile =
  match dependencies with
  | [] -> stockpile
  | h :: t ->
      let resource_name = Buildings.resource_name h in
      let resource_change = Buildings.resource_amount h in
      let new_resource_value =
        List.assoc resource_name stockpile - resource_change
      in
      let new_resources =
        List.map
          (fun (name, value) ->
            if name = resource_name then (name, new_resource_value)
            else (name, value))
          stockpile
      in
      sub_resource_helper t new_resources

let sub_resource building stockpile =
  let dependencies = Buildings.resource_dependency building in
  sub_resource_helper dependencies stockpile

(** [sub_maintance] aubtracts the money that the building generates to
    it's value in resource stockpile*)
let sub_maintance building (stockpile : stockpile) : stockpile =
  let new_money =
    List.assoc "money" stockpile - Buildings.maintenance building
  in
  List.map
    (fun (name, value) ->
      if name = "money" then (name, new_money) else (name, value))
    stockpile

(** [add_income] adds the money that the building generates to it's
    value in resource stockpile*)
let add_income building (stockpile : stockpile) : stockpile =
  let new_money =
    List.assoc "money" stockpile + Buildings.income building
  in
  List.map
    (fun (name, value) ->
      if name = "money" then (name, new_money) else (name, value))
    stockpile

(** [add_resources] adds the resource that the building generates to
    it's value in resource stockpile*)
let add_resources building (stockpile : stockpile) : stockpile =
  let resource = Buildings.output building in
  let resource_name = Buildings.resource_name resource in
  let resource_change = Buildings.resource_amount resource in
  let new_resource_value =
    List.assoc resource_name stockpile + resource_change
  in
  List.map
    (fun (name, value) ->
      if name = resource_name then (name, new_resource_value)
      else (name, value))
    stockpile

let rec update_resources (lst : building list) stockpile : stockpile =
  match lst with
  | [] -> stockpile
  | h :: t ->
      (* if not (resource_sufficiency_check h resource_stockpile) then
         update_resources t (sub_maintance h resource_stockpile) else
         update_resources t (add_resources h (add_income h (sub_resource
         h (sub_maintance h resource_stockpile)))) *)
      update_resources t stockpile

let next_state state =
  (* if pause then if state.tick < 100 then let new_resources =
     update_resources state.buildings state.stockpile in let
     update_state = new_state ~stockpile:new_resources ~tick:(state.tick
     + 1) (file_name state) (canvas_width state) (canvas_height state)
     (map_length state) (cell_width state) (cell_height state) in
     next_state update_state else next_state state else next_state state *)
  let new_resources =
    update_resources state.buildings state.stockpile
  in
  new_state ~stockpile:new_resources ~tick:(state.tick + 1)
    (filename state)
    (canvas_size state |> fst)
    (canvas_size state |> snd)
    (map_length state)
    (cell_size state |> fst)
    (cell_size state |> snd)

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
let generate_stock_lst stockpile =
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
      ("stockpile", `List (generate_stock_lst [] s.stockpile));
    ]
  |> Yojson.to_string
