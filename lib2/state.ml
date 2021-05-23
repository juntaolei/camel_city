open Buildings
open Js_of_ocaml
open Yojson.Basic.Util

type stockpile = resource list
type food_stockpile = (string * int) list
type food_type = (string * int) list

type cell =
  | Building of building
  | Road of road
  | None

type state = {
  mutable text : string;
  mutable file_name : string;
  mutable tick : int;
  mutable canvas_width : int;
  mutable canvas_height : int;
  mutable canvas_size : int * int;
  mutable map_length : int;
  mutable cell_size : int * int;
  mutable cells : cell array array;
  mutable population : int;
  mutable stockpile : stockpile;
  mutable buildings : building list;
  mutable selected_building : int;
  mutable happieness : float;
  mutable food : int;
  mutable game_over : bool;
  mutable food_stockpile : food_stockpile;
  mutable food_type: food_type;
  mutable houseing_capacity : int;
  mutable military_strength : int;
  mutable alavilble_pop : int;
  mutable money : int;
  mutable broke_counter : int;
  mutable starve_counter : int;
  mutable revolt_counter : int;
  mutable broke:bool;
  mutable starved:bool;
  mutable out_of_time:bool;
  mutable revolt:bool;
  mutable defeated:bool;
  mutable final_building:bool;
}
let broke_limit = 180
let starve_limit = 120
let revolt_limit = 300
let time_limit = 7200

let text state = state.text
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

let canvas_width state = fst state.canvas_size

let canvas_height state = snd state.canvas_size

let map_length state = state.map_length

let cell_size state = state.cell_size

let cell_width state = fst state.cell_size

let cell_height state = snd state.cell_size

let get_index id lst = List.nth lst id

let cells state = state.cells

let buildings state = state.buildings
let food_stockpile state = state.food_stockpile
let food_type state = state.food_type
let houseing_capacity state = state.houseing_capacity
let military_strength state = state.military_strength
let get_money state = state.money
let alavilble_pop state = state.alavilble_pop


let str_of_cell = function
  | None -> ""
  | Road _ -> "road"
  | Building b -> building_name b

let tick state = state.tick

let population state = state.population

let stockpile state = state.stockpile

let get_buildings state = state.buildings

let file_name state = state.file_name

let get_index id lst = List.nth lst id

let get_happieness state = state.happieness

let get_pop state = state.population

let get_food state = state.food

let is_game_over state = state.game_over

(** [iter_buildings acc json] is the list of initialized [buildings]
    extracted from the json object [json]. *)
let rec iter_buildings (acc : building list) (s : Yojson.Basic.t list) =
  match s with
  | [] -> acc
  | h :: t ->
      iter_buildings
        (new_building
           (h |> member "name" |> to_string)
           (h |> member "cost" |> to_string |> int_of_string)
           (h |> member "maintenance" |> to_string |> int_of_string)
           (h |> member "output" |> member "amount" |> to_string
          |> int_of_string)
           (h |> member "output" |> member "name" |> to_string)
           (h |> member "tax" |> to_string |> int_of_string)
           (h |> member "defense" |> to_string |> int_of_string)
           (h
           |> member "resource_dependency"
           |> to_list |> get_index 0 |> member "amount" |> to_string
           |> int_of_string)
           (h
           |> member "resource_dependency"
           |> to_list |> get_index 0 |> member "name" |> to_string)
        :: acc)
        t

let buildings_init =
  iter_buildings []
    ("buildings_init.json" |> Yojson.Basic.from_file
   |> member "buildings" |> to_list)

let new_state
    (text:string)
    (file_name : string)
    (stockpile : stockpile)
    (tick : int)
    (canvas_width : int)
    (canvas_height : int)
    (map_length : int)
    (cell_width : int)
    (cell_height : int)
    (buildings: building list)
    (population : int)
    (happieness : float)
    (game_over : bool)
    (food : int)
    (food_stockpile : food_stockpile)
    (food_type: food_type)
    (houseing_capacity : int)
    (military_strength : int)
    (alavilble_pop:int)
    (money:int)
    (broke_counter : int)
    (starve_counter : int)
    (revolt_counter : int)
    (broke:bool)
    (starved:bool)
    (out_of_time:bool)
    (revolt:bool)
    (defeated:bool)
    (final_building:bool)
    
  {
    text = text;
    tick = tick;
    file_name = file_name;
    canvas_width = canvas_width;
    canvas_height = canvas_height;
    canvas_size = (canvas_width, canvas_height);
    map_length;
    cell_size = (cell_width, cell_height);
    cells = build_cell_lst map_length map_length;
    stockpile = stockpile;
    buildings = buildings_init;
    population = population;
    selected_building = -1;
    food = food;
    happieness = happieness;
    game_over = game_over;
    food_stockpile = food_stockpile;
    food_type = food_type;
    houseing_capacity = houseing_capacity; 
    military_strength = military_strength;
    alavilble_pop = alavilble_pop;
    money= money;
    broke_counter = broke_counter;
    starve_counter = starve_counter;
    revolt_counter = revolt_counter;
    broke = broke;
    starved = starve;
    out_of_time = out_of_time;
    revolt = revolt;
    defeated = defeated;
    final_building = final_building;
  }
(*  *)
let rec houseing_capacity (buildings: building list) = 
  match buildings with
  | [] -> 0
  | h::t -> Buildings.housing h + houseing_capacity t

let rec defense_score (buildings: building list) = 
  match buildings with
  | [] -> 0
  | h::t -> Buildings.defense h + defense_score t

(* all functions below updates food *)
let rec food_update (food:food_stockpile) (stockpile:stockpile) = 
  match food with
  | [] -> []
  | h::t -> [(food , List.assoc (fst h) stockpile)] @ (food_update t stockpile)
let rec food_total_helper (food:food_stockpile) = 
  match food with
  | [] -> 0
  | h::t -> snd h + food_total_helper t
let food_total (food:food_stockpile) (population: int) = 
  let a = food_total_helper food in
  a - population 
(* change the food resouces in stockpile to account for the food eaten by the population *)
let rec food_update_sub  = failwith "please fill in Jun Tao"






let rec food_happieness_helper (food:food_stockpile) (food_type:food_type)  = 
  match food with
  | [] -> 0.0
  | h::t -> 
    let food_name = fst h in
    let food_amount = List.assoc food_name food in
    let food_happy = List.assoc food_name food_type in
    let happy_score = float_of_int (food_amount + food_happy) in
    happy_score +. food_happieness_helper t food_type 

let rec food_variety (food:food_stockpile) = 
  match food with
  | [] -> 0
  | h::t -> 
    let food_name = fst h in
    if List.assoc food_name food > 0 then food_variety t + 1 else food_variety t

let food_happieness (food:food_stockpile) (food_type:food_type) = 
    let variety_score = food_variety food in
    let food_score = food_happieness_helper food food_type  in
    float_of_int (variety_score) +. food_score
let homelessness (population: int) (houseing_capacity : int) =
  let homeless_camels = population - houseing_capacity in
  if homeless_camels < 1 then 0 else homeless_camels

let entertainment (buildings: building list) = 
  match buildings with
  | [] -> 0
  | h::t -> Buildings.happieness h + entertainment t


let total_happy (buildings: building list) (food:food_stockpile) (food_type:food_type) (population:int) (pop_capacity:int)  =
  let food_happy = food_happieness food food_type in
  let entertainment_happy = float_of_int (entertainment buildings) in
  let homeless_score = float_of_int (homelessness population housing_capacity) in
  let pop_score = float_of_int population *. 0.1
  50.0 + food_happy +. entertainment_happy +. homeless_score +. pop_score

let revolt_count (happieness:float) (revolt_counter:int) = if happieness < 30.0 then revolt_counter + 1 else 0
let revolt = (revolt_limit:int) (revolt_counter:int) = if happy_counter < revolt_limit then false else true


let happieness_effect (happieness:float) =
  if happieness < 30.0 then -20
  else if happieness < 40.0 then -10
  else if happieness < 60.0 then 10
  else 20

let working_pops (buildings: building list) = 
match buildings with
| [] -> 0
| h::t -> Buildings.get_pop_dependency h + working_pops t
(* how many unemployed camels there are. needs to be checked aginst building pop depenancy. if less then pop dependencey
  don't build. if greater build and run th *)
let alavilble_pop (population:int) (working_pops:int) =
  population - working_pops

let starve_count food_total starve_counter = if food_total < 0 then 1 + starve_counter else 0
let starving (food_counter:int) (food_limit:int) = if food_counter < food_limit then false else true
let defeated = false
let out_of_time tick tick_limit = if tick < tick_limit then false else true

let broke_count (money:int) (broke_counter:int) = if money < 0 then 1 + broke_counter else 0
let broke broke_counter broke_limit= if broke_counter < broke_limit then false else true

let game_over (broke:bool) (starve:bool) (out_of_time:bool) (revolt:bool) (defeated:bool) (game_over_building:bool) =
  if broke || starve || out_of_time || revolt || defeated || game_over_building then true else false
let game_over_text (broke:bool) (starve:bool) (out_of_time:bool) (revolt:bool) (defeated:bool) (game_over_building:bool) =
  if not broke then "1"
  else if not starve then "2"  
  else if not out_of_time then "3"
  else if not revolt then "4"
  else if not defeated then "5"
  else if not game_over_building then "6"
  else ""

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
        update_resources er
          (add_resources h
             (add_income h
                (sub_resource h (sub_maintance h resource_stockpile))))
let pop_increase (tick:int) (population:int) = if tick mod 60 = 0 then population + 5 else population


(* makes the next state of the game. *)
let next_state (state : state) : state =
  let a = starve_count state.food state.starve_counter in
  let is_starved  = starving a starve_limit in
  let b = broke_count state.money state.brokecounter in
  let is_broke = broke b broke_limit in
  let is_out_of_time = out_of_time state.tick tick_limit in
  let c = revolt_count state.happieness state.revolt_count in
  let is_revolt = revolt c revolt_limit in
  let defeated = false
  let final_building = false
  let game_over_flag = game_over is_broke is_starved is_out_of_time is_revolt defeated final_building in
  let game_over_discription = game_over_text is_broke is_starved is_out_of_time is_revolt defeated final_building in
  if game_over_flag then
    let final_state = new_state game_over_discription state.filename state.stockpile state.tick 
    state.canvas_width state.canvas_height state.map_length state.cell_width state.cell_height
    state.buildings state.population state.happieness state.food game_over_flag state.food_stockpile
    state.food_type state.houseing_capacity state.military_strength state.alavilble_pop state.money a b c
    is_broke is_starved is_out_of_time is_revolt defeated final_building in
    final_state
    else
      let buildings = state.buildings in
      let stockpile = state.stockpile in
      let food_stockpile = state.food_stockpile in
      let housing' = houseing_capacity buildings in
      let defense' = defense_score buildings in
      let stockpile' = update_resources buildings stockpile in
      let food_stockpile' =  food_update food_stockpile stockpile' in
      let food_total' = food_total food_stockpile state.population  in
      let happiness' = total_happy buildings food_stockpile' state.food_type state.population housing' in
      let stockpile'' =  food_update_sub in 
      let unemployed_pop = alavilble_pop state.population buildings in
      let pop' = pop_increase state.tick state.population in
      let defense_bonus = happieness_effect happiness' in
      let defense'' = defense' + defense_bonus in
      let tick' = state.tick + 1 in
      let text' = game_over_discription in
      let updated_state = new_state game_over_discription state.filename stockpile'' tick' 
      state.canvas_width state.canvas_height state.map_length state.cell_width state.cell_height
      buildings pop' happiness' food_total' game_over_flag food_stockpile'
      state.food_type housing' defense'' unemployed_pop state.money a b c
      is_broke is_starved is_out_of_time is_revolt defeated final_building in
      updated_state




let rec state_update_timer (interval_time:float) (last_update_time:float) (state : state)  = 
  let update_interval = interval_time in
  let inital_time = last_update_time in
  let update_time = inital_time +. update_interval in
  let current_time = Unix.gettimeofday () in
  if current_time > update_time 
    then 
      let updated_state = next_state state in
      state_update_timer interval_time current_time updated_state
    else state_update_timer interval_time inital_time state

(** [init_stockpile acc json] is the list of initialized [stockpile]
    extracted from the json object [json]. *)
let rec init_stockpile (acc : resource list) = function
  | [] -> acc
  | h :: t ->
      init_stockpile
        (new_resource
           (h |> member "name" |> to_string)
           (h |> member "amount" |> to_string |> int_of_string)
        :: acc)
        t

(** [init_new_building name bld_lst] is a new building with name [name]
    if such building exists in [blk_lst]. Otherwise a failure is raised. *)
let rec init_new_building name = function
  | [] -> failwith "not a valid building name"
  | h :: t ->
      if building_name h = name then h else init_new_building name t

(** [iter_cells s acc json] is the updated cells from information in
    [json]. *)
let rec iter_cells s (acc : cell array array) = function
  | [] -> acc
  | h :: t ->
      let x_coord = h |> member "x" |> to_string |> int_of_string in
      let y_coord = h |> member "y" |> to_string |> int_of_string in
      let obj = h |> member "object" |> to_string in
      if not (obj = "") then
        if obj = "road" then
          acc.(x_coord).(y_coord) <- Road (new_road 1 x_coord y_coord)
          (* cost of roads? *)
        else
          acc.(x_coord).(y_coord) <-
            Building (init_new_building obj s.buildings);
      iter_cells s acc t

let from_file file =
  let json = Yojson.Basic.from_file file in
  let get_field name coord =
    json |> member name |> member coord |> to_string |> int_of_string
  in
  let init_state =
    new_state file
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
      List.rev
        (init_stockpile [] (json |> member "stockpile" |> to_list));
    population =
      json |> member "population" |> to_string |> int_of_string;
  }

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
      List.rev
        (init_stockpile [] (json |> member "stockpile" |> to_list));
    population =
      json |> member "population" |> to_string |> int_of_string;
  }

(** [generate_stock_lst acc pile] is the `List containing contents of
    [pile]. *)
let rec generate_stock_lst acc pile =
  match pile with
  | [] -> List.rev acc
  | h :: t ->
      generate_stock_lst
        (`Assoc
           [
             ("name", `String (resource_name h));
             ("amount", `String (resource_amount h |> string_of_int));
           ]
        :: acc)
        t

(** [generate_cell_lst cells] is the `List containing contents of
    [pile]. *)
let generate_cell_lst cells =
  let str_of_cell (cel : cell) : string =
    match cel with
    | None -> ""
    | Building t -> building_name t
    | Road _ -> "road"
  in
  let acc_row = [] in
  let acc = [] in
  Array.fold_right
    (fun x acc_row ->
      Array.fold_right
        (fun y acc ->
          `Assoc
            [
              ("x", `String (List.length acc_row |> string_of_int));
              ("y", `String (List.length acc |> string_of_int));
              ("object", `String (str_of_cell y));
            ]
          :: acc)
        x acc
      @ acc_row)
    cells acc_row

let save_state s =
  let json_obj =
    `Assoc
      [
        ("tick", `String (string_of_int (tick s)));
        ( "canvas_size",
          `Assoc
            [
              ("x", `String (string_of_int (canvas_width s)));
              ("y", `String (string_of_int (canvas_height s)));
            ] );
        ("map_length", `String (string_of_int (map_length s)));
        ( "cell_size",
          `Assoc
            [
              ("x", `String (string_of_int (cell_width s)));
              ("y", `String (string_of_int (cell_height s)));
            ] );
        ("cells", `List (generate_cell_lst s.cells));
        ("population", `String (string_of_int (population s)));
        ("stockpile", `List (generate_stock_lst [] s.stockpile));
      ]
  in
  (* Yojson.to_file s.file_name json_obj *)
  Yojson.to_string json_obj
