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

let select_building state i = state.selected_building <- i

let selected_building state = state.selected_building >= 0
  
(** [build_cell_lst width height] is an two dimensional array with
    [width] and [height]. *)
let build_cell_lst width height = Array.make_matrix width height None

(** [place_cell state cell x y] updates the old cell as indexed by [x]
    and [y] in [state] with a new [cell]. *)
let place_cell state cell x y = state.cells.(x).(y) <- cell

let canvas_size state = state.canvas_size

let canvas_width state = fst state.canvas_size

let canvas_height state = snd state.canvas_size

let map_length state = state.map_length

let cell_size state = state.cell_size

let cell_width state = fst state.cell_size

let cell_height state = snd state.cell_size

let get_index id lst = List.nth lst id

let cells state = state.cells

let str_of_cell (c : cell) = match c with
| None -> ""
| Road r -> "road"
| Building b -> building_name b

let tick state = state.tick

let population state = state.population

let stockpile state = state.stockpile

let get_buildings state = state.buildings

let file_name state = state.file_name

let get_index id lst = List.nth lst id

(** [iter_buildings acc json] is the list of initialized [buildings]
    extracted from the json object [json]. *)
let rec iter_buildings (acc : building list) (s : Yojson.Basic.t list) = 
  match s with
| [] -> acc
| h :: t -> iter_buildings ((new_building 
  (h |> member "name" |> to_string)
  (h |> member "cost" |> to_string |> int_of_string)
  (h |> member "maintenance" |> to_string |> int_of_string)
  (h |> member "output" |> member "amount" |> to_string |> int_of_string)
  (h |> member "output" |> member "name" |> to_string)
  (h |> member "tax" |> to_string |> int_of_string)
  (h |> member "defense" |> to_string |> int_of_string)
  (h |> member "resource_dependency" |> to_list |> get_index 0
    |> member "amount" |> to_string |> int_of_string)
  (h |> member "resource_dependency" |> to_list |> get_index 0
    |> member "name" |> to_string)
) :: acc) t

let new_state
    (filename : string)
    ?(stockpile = [])
    ?(buildings = []) (*?*)
    ?(tick = 1)
    (canvas_width : int)
    (canvas_height : int)
    (map_length : int)
    (cell_width : int)
    (cell_height : int) =
  {
    tick;
    file_name = filename;
    canvas_size = (canvas_width, canvas_height);
    map_length;
    cell_size = (cell_width, cell_height);
    cells = build_cell_lst map_length map_length;
    stockpile;
    buildings = iter_buildings [] 
      ((Yojson.Basic.from_file "buildings_init.json") 
      |> member "buildings" |> to_list);
    population = 0;
    selected_building = -1;
  }

let rec resource_sufficiency_check_helper
    (dependencies : resource list)
    (resource_stockpile : stockpile) : bool =
  match dependencies with
  | [] -> true
  | h :: t ->
      let resource_name = Buildings.resource_name h in
      let resource_change = Buildings.resource_amount h in
      let new_resource_value =
        List.assoc resource_name resource_stockpile - resource_change
      in
      if new_resource_value >= 0 then
        true && resource_sufficiency_check_helper t resource_stockpile
      else
        false && resource_sufficiency_check_helper t resource_stockpile

let resource_sufficiency_check
    (building : building)
    (resource_stockpile : stockpile) : bool =
  let dependencies = Buildings.resource_dependency building in
  resource_sufficiency_check_helper dependencies resource_stockpile

(** [sub_resource] subtracts the resources that the building uses from
    their values in resource stockpile*)
let rec sub_resource_helper
    (dependencies : resource list)
    (resource_stockpile : stockpile) : stockpile =
  match dependencies with
  | [] -> resource_stockpile
  | h :: t ->
      let resource_name = Buildings.resource_name h in
      let resource_change = Buildings.resource_amount h in
      let new_resource_value =
        List.assoc resource_name resource_stockpile - resource_change
      in
      let new_resources =
        List.map
          (fun (name, value) ->
            if name = resource_name then (name, new_resource_value)
            else (name, value))
          resource_stockpile
      in
      sub_resource_helper t new_resources

let sub_resource (building : building) (resource_stockpile : stockpile)
    : stockpile =
  let dependencies = Buildings.resource_dependency building in
  sub_resource_helper dependencies resource_stockpile

(** [sub_maintance] aubtracts the money that the building generates to
    it's value in resource stockpile*)
let sub_maintance (building : building) (resource_stockpile : stockpile)
    : stockpile =
  let new_money =
    List.assoc "money" resource_stockpile
    - Buildings.maintenance building
  in
  List.map
    (fun (name, value) ->
      if name = "money" then (name, new_money) else (name, value))
    resource_stockpile

(** [add_income] adds the money that the building generates to it's
    value in resource stockpile*)
let add_income (building : building) (resource_stockpile : stockpile) :
    stockpile =
  let new_money =
    List.assoc "money" resource_stockpile + Buildings.income building
  in
  List.map
    (fun (name, value) ->
      if name = "money" then (name, new_money) else (name, value))
    resource_stockpile

(** [add_resources] adds the resource that the building generates to
    it's value in resource stockpile*)
let add_resources (building : building) (resource_stockpile : stockpile)
    : stockpile =
  let resource = Buildings.output building in
  let resource_name = Buildings.resource_name resource in
  let resource_change = Buildings.resource_amount resource in
  let new_resource_value =
    List.assoc resource_name resource_stockpile + resource_change
  in
  List.map
    (fun (name, value) ->
      if name = resource_name then (name, new_resource_value)
      else (name, value))
    resource_stockpile

let rec update_resources
    (building_list : building list)
    (resource_stockpile : stockpile) : stockpile =
  match building_list with
  | [] -> resource_stockpile
  | h :: t ->
      if not (resource_sufficiency_check h resource_stockpile) then
        update_resources t (sub_maintance h resource_stockpile)
      else
        update_resources t
          (add_resources h
             (add_income h
                (sub_resource h (sub_maintance h resource_stockpile))))

let rec next_state (state : state) : state =
  if pause then
    if state.tick < 100 then
      let new_resources =
        update_resources state.buildings state.stockpile
      in
      let update_state =
        new_state ~stockpile:new_resources
          ~buildings:(get_buildings state) ~tick:(state.tick + 1)
          (file_name state) (canvas_width state) (canvas_height state) (map_length state)
          (cell_width state) (cell_height state)
      in

      next_state update_state
    else next_state state
  else next_state state


(** [init_stockpile acc json] is the list of initialized [stockpile]
    extracted from the json object [json]. *)
let rec init_stockpile (acc : resource list) = function
| [] -> acc
| h :: t -> init_stockpile ((new_resource 
  (h |> member "name" |> to_string)
  (h |> member "amount" |> to_string |> int_of_string)
  ) :: acc) t

(** [init_new_building name bld_lst] is a new building with name [name] if
    such building exists in [blk_lst]. Otherwise a failure is raised. *)
let rec init_new_building name = function
| [] -> failwith "not a valid building name"
| h :: t -> 
  if (building_name h) = name then h 
  else init_new_building name t

(** [iter_cells s acc json] is the updated cells from information in [json]. *)
let rec iter_cells s (acc : cell array array) = function
| [] -> acc
| h :: t ->
  let x_coord = h |> member "x" |> to_string |> int_of_string in
  let y_coord = h |> member "y" |> to_string |> int_of_string in
  let obj = h |> member "object" |> to_string in
  (if not (obj = "") then 
    if (obj = "road") then 
      acc.(x_coord).(y_coord) <- Road (new_road 1 x_coord y_coord) (* cost of roads? *)
    else
    acc.(x_coord).(y_coord) <- Building (init_new_building obj s.buildings));
  iter_cells s acc t

let from_file file = 
  let json = Yojson.Basic.from_file file in
  let get_field name coord = (json |> member name |> member coord 
  |> to_string |> int_of_string) in
  let init_state = new_state file 
    (get_field "canvas_size" "x") 
    (get_field "canvas_size" "y")
    (json |> member "map_length" |> to_string |> int_of_string)
    (get_field "cell_size" "x") 
    (get_field "cell_size" "y")
  in 
  {
    init_state with 
    cells = iter_cells init_state init_state.cells 
      (json |> member "cells" |> to_list);
    stockpile = List.rev 
      (init_stockpile [] (json |> member "stockpile" |> to_list));
    population = (json |> member "population" |> to_string |> int_of_string)
  }

(** [generate_stock_lst acc pile] is the `List containing contents 
of [pile]. *)
let rec generate_stock_lst acc pile = match pile with
  | [] -> List.rev acc
  | h :: t -> generate_stock_lst
    (`Assoc 
      [("name", `String (resource_name h));
      ("amount", `String (resource_amount h |> string_of_int))] :: acc) t

(** [generate_cell_lst cells] is the `List containing contents 
of [pile]. *)
let generate_cell_lst cells =
  let str_of_cell (cel : cell) : string = match cel with
    | None -> ""
    | Building t -> building_name t
    | Road r -> "road"
  in
  let acc_row = [] in 
  let acc = [] in
    Array.fold_right (fun x acc_row ->
        (Array.fold_right 
        (fun y acc -> 
          `Assoc [("x", `String (List.length acc_row |> string_of_int));
          ("y", `String (List.length acc |> string_of_int));
          ("object", `String (str_of_cell y))] :: acc) x acc) @ acc_row
        ) cells acc_row

let save_state s = 
  let json_obj = 
    `Assoc [ 
    ("tick", `String (string_of_int (tick s)));
    ("canvas_size", `Assoc [
      ("x", `String (string_of_int (canvas_width s)));
      ("y", `String (string_of_int (canvas_height s)))
    ]);
    ("map_length", `String (string_of_int (map_length s)));
    ("cell_size", `Assoc [
      ("x", `String (string_of_int (cell_width s)));
      ("y", `String (string_of_int (cell_height s)))
    ]);
    ("cells", `List (generate_cell_lst s.cells));
    ("population", `String (string_of_int (population s)));
    ("stockpile", `List (generate_stock_lst [] s.stockpile))
    ]
  in
  Yojson.to_file s.file_name json_obj