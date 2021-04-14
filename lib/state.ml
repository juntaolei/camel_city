open Buildings
open Yojson.Basic.Util

(* For now: index 0 = oat, index 1 = electricity, index 2 = iron, index
   3 = money *)
type stockpile = resource list

type update = { stockpile : stockpile }

type cell =
  | Building of building
  | Road_t of road
  | None

type state = {
  file_name : string;
  tick : int;
  canvas_size : int * int;
  map_length : int;
  cell_size : int * int;
  cells : cell array array;
  population : int;
  stockpile : stockpile;
  buildings : building list;
  mutable selected_building : int;
}

(** [iter_buildings acc json] is the list of initialized [buildings]
    extracted from the json object [json]. *)
let rec iter_buildings (acc : building list) = function
| [] -> acc
| h :: t -> iter_buildings ((new_building 
  h |> member "name" |> to_string
  h |> member "cost" |> to_string |> int_of_string
  h |> member "output" |> member "amount" |> to_string |> int_of_string
  h |> member "output" |> member "name" |> to_string
  h |> member "tax" |> to_string |> int_of_string
  h |> member "cost" |> to_string |> int_of_string
  h |> member "defense" |> to_string |> int_of_string
  h |> member "resource_dependency" |> member "amount" |> to_string |> int_of_string
  h |> member "resource_dependency" |> member "name" |> to_string
) :: acc) t

(** [init_stockpile acc json] is the list of initialized [stockpile]
    extracted from the json object [json]. *)
let rec init_stockpile (acc : resource list) = function
| [] -> acc
| h :: t -> init_stockpile ((new_resource 
  h |> member "amount" |> to_string |> int_of_string
  h |> member "name" |> to_string
  ) :: acc) t

(** [init_new_building name bld_lst] is a new building with name [name] if
    such building exists in [blk_lst]. Otherwise a failure is raised. *)
let rec init_new_building name = function
| [] -> failwith "not a valid building name"
| h :: t -> if (building_name h) = name then h 
  else init_new_building name t

(** [iter_cells s acc json] is the updated cells from information in [json]. *)
let rec iter_cells s (acc : cell array array) = function
| [] -> acc
| h :: t ->
  let x_coord = h |> member "x" |> to_string |> int_of_string in
  let y_coord = h |> member "y" |> to_string |> int_of_string in
  let obj = h |> member "object" |> to_string in
  if not (obj = "") then 
    acc.(x_coord).(y_coord) <- Building (init_new_building obj s.buildings);
  iter_cells acc t

(** [from_json json] is the state read from the file with name [file]. *)
let from_file file = 
  let json = Yojson.Basic.from_file file in
  let get_field name coord = json |> member name |> member coord 
  |> to_string |> int_of_string in
  let init_state = new_state file 
    (get_field "canvas_size" "x") 
    (get_field "canvas_size" "y")
    (json |> member "map_length" |> to_string)
    (get_field "cell_size" "x") 
    (get_field "cell_size" "y")
  in 
  {
    init_state with 
    cells = iter_cells init_state init_state.cells 
      json |> member "cells" |> to_list;
    stockpile = List.rev 
      (init_stockpile [] json |> member "stockpile" |> to_list);
    population = json |> member "population" |> to_string |> int_of_string;
    buildings = iter_buildings [] 
      (Yojson.Basic.from_file "buildings_init.json") 
      |> member "buildings" |> to_list;
    }


let select_building state i = state.selected_building <- i

let selected_building state = state.selected_building >= 0

(** [build_cell_lst width height] is an two dimensional array with
    [width] and [height]. *)
let build_cell_lst width height = Array.make_matrix width height None

(** [place_cell state cell x y] updates the old cell as indexed by [x]
    and [y] in [state] with a new [cell]. *)
let place_cell state cell x y = state.cells.(x).(y) <- cell

(** [total_tax_amount state] is the total tax amount from all cells in
    [state]. *)
let total_tax_amount state =
  Array.fold_left
    (fun acc row ->
      acc
      + Array.fold_left
          (fun acc ele ->
            match ele with Building b -> tax_amount b + acc | _ -> acc)
          0 row)
    0 state.cells

(** [merge_stockpile s1 s2] is combines stockpiles [s1] and [s2] into a
    single stockpile. *)
let merge_stockpile s1 s2 =
  List.map2
    (fun e1 e2 ->
      new_resource (resource_name e1)
        (resource_amount e1 + resource_amount e2))
    s1 s2

(** [update_tax pile] is the [pile] after collecting tax in the game
    [state]. *)
let update_tax state stockpile =
  let taxes =
    [
      new_resource "" 0;
      new_resource "" 0;
      new_resource "" 0;
      new_resource "money" (total_tax_amount state);
    ]
  in
  merge_stockpile stockpile taxes

(* order of update? option 1: geographic location (recursion through the
   cell list list) option 2: oat_plantation-> power_plant -> mine (so
   that the resource produced can be used as inputs for other buildlings
   immediately in the same round of update) maybe consider this for MS2? *)
let update_state = failwith "unimplemented"

let new_state
    file_name
    canvas_width
    canvas_height
    map_length
    cell_width
    cell_height =
  {
    file_name = file_name;
    tick = 1;
    canvas_size = (canvas_width, canvas_height);
    map_length;
    cell_size = (cell_width, cell_height);
    cells = build_cell_lst map_length map_length;
    stockpile = [];
    buildings = [];
    population = 0;
    selected_building = -1;
  }

let canvas_size state = state.canvas_size

let map_length state = state.map_length

let file_name state = state.file_name;

let cell_size state = state.cell_size

let cells state = state.cells

let next_state state (update : update) =
  {
    state with
    stockpile = merge_stockpile state.stockpile update.stockpile;
  }
