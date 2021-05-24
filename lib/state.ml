open Buildings
open Yojson.Basic.Util

(** [default_stockpile] is the stockpile on any fresh game sessions. *)
let default_stockpile =
  [
    ("money", 200);
    ("electricity", 0);
    ("food", 100);
    ("iron", 0);
    ("coal", 0);
    ("steel", 0);
    ("canned oats", 0)
  ]

(** [deficit_limit] is the maximum number of ticks before resource deficiency
    causes the game to end. *)
let deficit_limit = 20

(** [starvation_limit] is the maximum number of ticks before food deficiency
    causes the game to end. *)
let starvation_limit = 20

(** [time_limit] is the maximum number of ticks before the game ends. *)
let time_limit = 400

type cell =
  | Building of building
  | Road of road
  | None

type state = {
  mutable tick : int;
  mutable interval_time : float;
  mutable last_updated : float;
  mutable text : string;
  mutable canvas_size : int * int;
  mutable map_length : int;
  mutable cell_size : int * int;
  mutable buildings : building list;
  mutable cells : cell array array;
  mutable selected_cell : int;
  mutable housing_capacity : int;
  (*mutable military_strength : int;*)
  mutable population : int;
  mutable unemployed :int;
  mutable food : int;
  mutable deficit_counter : int;
  mutable starvation_counter : int;
  (*mutable revolt_counter : int;*)
  (*mutable happiness : float;*)
  mutable is_paused : bool;
  mutable is_game_over : bool;
  mutable condition : int;
  mutable is_final_building_placed : bool;
  mutable game_over_message : string;
  mutable stockpile : (string * int) list;
}

let str_of_cell = function
  | None -> ""
  | Road _ -> "road"
  | Building b -> b.name

let select_cell state i = state.selected_cell <- i

let is_selected state = state.selected_cell >= 0

let place_cell state cell x y = state.cells.(x).(y) <- cell

(** [build_cell_lst width height] is an two dimensional array with
    [width] and [height]. *)
let build_cell_lst width height = Array.make_matrix width height None

(** [iter_buildings acc json] is the list of initialized [buildings]
    extracted from the json object [json]. *)
let iter_buildings json =
  List.fold_left
    (fun acc building ->
      let int_of_member name = building |> member name |> to_int in
      let name = building |> member "name" |> to_string in
      let cost = int_of_member "cost" in
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
      (*let happiness = int_of_member "happiness" in*)
      let population_dependency =
        int_of_member "population_dependency"
      in
      let housing = int_of_member "housing" in
      (*let entertainment = int_of_member "entertainment" in*)
      let is_final_building =
        building |> member "is_final_building" |> to_bool
      in
      new_building name cost maintenance output tax defense
        resource_dependency population_dependency housing
        is_final_building
      :: acc)
    [] json

(** [buildings_init] is a list of buildings initialized from a
    configuration building list JSON file. *)
let default_buildings =
  "buildings_init.json" |> Yojson.Basic.from_file |> member "buildings"
  |> to_list |> iter_buildings

let new_state
    ?(tick = 1)
    ?(housing_capacity = 0)
    (*?(military_strength = 0)*)
    ?(population = 0)
    ?(unemployed = 0)
    ?(food = 0)
    ?(deficit_counter = 0)
    ?(starvation_counter = 0)
    (*?(revolt_counter = 0)*)
    (*?(happiness = 0.)*)
    ?(is_paused = true)
    ?(is_game_over = false)
    ?(condition = 0)
    ?(is_final_building_placed = false)
    canvas_width
    canvas_height
    map_length
    cell_width
    cell_height =
  {
    tick;
    interval_time = 1.;
    last_updated = 0.;
    text = "";
    canvas_size = (canvas_width, canvas_height);
    map_length;
    cell_size = (cell_width, cell_height);
    buildings = default_buildings;
    cells = build_cell_lst map_length map_length;
    selected_cell = -1;
    housing_capacity;
    (*military_strength;*)
    population;
    unemployed;
    food;
    (*happiness;*)
    deficit_counter;
    starvation_counter;
    (*revolt_counter;*)
    is_paused;
    is_game_over;
    condition;
    is_final_building_placed;
    game_over_message = "";
    stockpile = default_stockpile;
  }

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

(** [availabel_homes st] is the population capacity in [st]. *)
let available_homes state =
  Array.fold_left
    (fun acc row ->
      Array.fold_left
        (fun acc cell ->
          match cell with
          | Building b when b.name = "house" -> b.housing + acc
          | _ -> acc)
        0 row
      + acc)
    0 state.cells

let update_housing state = (* not sure if this is used anymore *)
  let housing_capacity =
    List.fold_left
      (fun acc building -> building.housing + acc) 0
      (available_buildings state)
  in
  state.housing_capacity <- housing_capacity

(** [update_food st] updates the amount of food after the camel population
    consumes their daily requirements. Each unit of camel consumes one unit
    of food every day/tick. *)
let update_food state =
  state.stockpile <- List.map
  (fun res ->
    if resource_name res = "food"
    then new_resource "food" (resource_amount res - state.population)
    else res) 
    state.stockpile;
  state.food <-
    List.find (fun (name, _) -> name = "food") state.stockpile
    |> resource_amount

(** [update_population st] updates the [population] and [unemployed] parameters
    in state [st]. Natural rate of population growth is [0.2]. *)
let update_population state =
  if state.tick mod 5 = 0 then
    let homes = available_homes state in
    let incr = int_of_float (0.2 *. float_of_int homes) in
    state.population <- state.population + incr;
    state.unemployed <- state.unemployed + incr
    (*
    let homes = List.length (available_homes state) in
    state.population <- state.population + homes;
    state.unemployed <- state.unemployed + homes;
    *)

(** [update_starvation_counter st] updates the [starvation_counter] of [st]. *)
let update_starvation_counter state =
  if state.food < 0 then
    state.starvation_counter <- state.starvation_counter + 1
  else if state.starvation_counter > 0 then
    state.starvation_counter <- 0
    
(** [check_resource_deficiency lst] checks if any resource if [lst], expect
    food, has reached a negative value. *)
let rec check_resource_deficiency = function
| [] -> true
| h :: t -> if resource_amount h < 0 && resource_name h <> "food" then false 
  else check_resource_deficiency t

(** [update_deficit_counter st] updates the [deficit_counter] of [st]. *)
let update_deficit_counter state =
  if not (check_resource_deficiency state.stockpile) then
    state.deficit_counter <- state.deficit_counter + 1
  else if state.deficit_counter > 0 then
    state.deficit_counter <- 0

(** [update_is_out_of_time state] updates the [condition] of [state] if tick 
    has exceed the time limit. *)
let update_is_out_of_time state =
   if state.tick > time_limit then state.condition <- 1

(** [update_is_int_starvation state] updates the [condition] of [state] if 
    [starvation_counter] has exceeded its limit. *)
let update_is_in_starvation state =
  if state.starvation_counter > starvation_limit then 
    state.condition <- 2

(** [update_is_in_decific state] updates the [condition] of [state] if 
    [deficit_counter] has exceeded its limit. *)
let update_is_in_deficit state =
  if state.deficit_counter > deficit_limit then 
    state.condition <- 3

(** [update_is_game_over state] updates the [is_game_over] parameter of 
    [state]. *)
let update_is_game_over state =
  if state.condition <> 0
  then state.is_game_over <- true

(** [update_game_over_text state] updates [text] of [state] corresponding to
    different types of game over situations. *)
let update_game_over_text state =
  let message =
    let cond = state.condition in
    if cond = 1 then "You have taken too long and the camel people have had enough of your leadership and
     they have all abandoned you."
    else if cond = 2 then "Without food your people have starved to death. 
      Camel City now lies abandoned; littered with skeletons"
    else if cond = 3 then "Your city is bankrupted. In desperate need to pay off the debt, you sold 
      Camel City to the nation of Java Cafe, whose upper classes use it as a source of cheap exploitable labor."
    else if state.is_final_building_placed then "By building the wonder you have resurected the great camel prophet, 
      The Clarkson. He shall now lead your people to victory against the Python Empire. You have won and saved camelkind"
    else ""
  in
  if state.is_game_over then state.game_over_message <- message

(** [is_stockpile_sufficient building stockiple] will check if the
    requirements for a building will make the values of [stockpile] to
    be negative. *)
let is_stockpile_sufficient building stockpile : bool =
  building.resource_dependency
  |> List.fold_left
       (fun acc dependency ->
         let name = resource_name dependency in
         let amount = resource_amount dependency in
         let new_amount = List.assoc name stockpile - amount in
         if new_amount >= 0 then acc && true else acc && false)
       true

(** [subtract_maintenace building stockpile] subtracts the money that
    the building generates to it's value in resource stockpile*)
let subtract_maintenace building stockpile =
  let new_money = List.assoc "money" stockpile - building.maintenance in
  let filter_money (name, value) =
    if name = "money" then (name, new_money) else (name, value)
  in
  List.map filter_money stockpile

(** [subtract_dependency building stockpile] subtracts the required
    resources that is needed by [building] from the [stockpile]. *)
let subtract_dependency building stockpile =
  let resource_dependency = building.resource_dependency in
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
let add_income building stockpile =
  let new_money = List.assoc "money" stockpile + building.income in
  List.map
    (fun (name, value) ->
      if name = "money" then (name, new_money) else (name, value))
    stockpile

(** [add_resources] adds the resource that the building generates to
    it's value in resource stockpile*)
let add_resource building stockpile =
  let output = building.output in
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
  List.find (fun building -> building.name = name) lst

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
          cells.(x cell).(y cell) <- Road (new_road (x cell) (y cell))
        else
          cells.(x cell).(y cell) <-
            Building (init_new_building (obj cell) state.buildings))
    json;
  cells

(** [json_of_assoc_lst lst] is the `List containing contents of an
    association [lst]. *)
let json_of_assoc_lst lst =
  let json_of_resource resource =
    `Assoc
      [
        ("name", `String (resource_name resource));
        ("amount", `Int (resource_amount resource));
      ]
  in
  List.fold_left
    (fun acc resource -> json_of_resource resource :: acc)
    lst

(** [generate_cell_lst cells] is the `List containing contents of
    [pile]. *)
let generate_cell_lst cells =
  let str_of_cell = function
    | None -> ""
    | Building building -> building.name
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

let from_string file_string =
  let json = Yojson.Basic.from_string file_string in
  let get_field name coordinate =
    json |> member name |> member coordinate |> to_int
  in
  let int_of_member name = json |> member name |> to_int in
  (*let float_of_member name = json |> member name |> to_float in*)
  let bool_of_member name = json |> member name |> to_bool in
  let init_state =
    new_state ~tick:(int_of_member "tick")
      ~housing_capacity:(int_of_member "housing_capacity")
      (*~military_strength:(int_of_member "military_strength")*)
      ~population:(int_of_member "population")
      ~unemployed:(int_of_member "unemployed")
      ~food:(int_of_member "food")
      ~deficit_counter:(int_of_member "deficit_counter")
      ~starvation_counter:(int_of_member "starvation_counter")
      (*~revolt_counter:(int_of_member "revolt_counter")*)
      (*~happiness:(float_of_member "happiness")*)
      ~is_paused:true
      ~is_game_over:(bool_of_member "is_game_over")
      ~condition:(int_of_member "condition")
      ~is_final_building_placed:
        (bool_of_member "is_final_building_placed")
      (get_field "canvas_size" "x")
      (get_field "canvas_size" "y")
      (int_of_member "map_length")
      (get_field "cell_size" "x")
      (get_field "cell_size" "y")
  in
  let cells =
    json |> member "cells" |> to_list
    |> iter_cells init_state init_state.cells
  in
  let stockpile =
    json |> member "stockpile" |> to_list |> init_stockpile |> List.rev
  in
  { init_state with cells; stockpile }

let save_state state =
  `Assoc
    [
      ("tick", `Int state.tick);
      ( "canvas_size",
        `Assoc
          [
            ("x", `Int (state.canvas_size |> fst));
            ("y", `Int (state.canvas_size |> snd));
          ] );
      ("map_length", `Int state.map_length);
      ( "cell_size",
        `Assoc
          [
            ("x", `Int (state.cell_size |> fst));
            ("y", `Int (state.cell_size |> snd));
          ] );
      ("cells", `List (generate_cell_lst state.cells));
      ("housing_capacity", `Int state.housing_capacity);
      (*("military_strength", `Int state.military_strength);*)
      ("population", `Int state.population);
      ("unemployed", `Int state.unemployed);
      ("food", `Int state.food);
      (*("happiness", `Float state.happiness);*)
      ("deficit_counter", `Int state.deficit_counter);
      ("starvation_counter", `Int state.starvation_counter);
      (*("revolt_counter", `Int state.revolt_counter);*)
      ("is_paused", `Bool state.is_paused);
      ("is_game_over", `Bool state.is_game_over);
      ("condition", `Int state.condition);
      ("is_final_building_placed", `Bool state.is_final_building_placed);
      ("stockpile", `List (json_of_assoc_lst [] state.stockpile));
    ]
  |> Yojson.to_string

(** [iter_resources lst] is the resource list extracted from the json
    list [lst]. *)
let iter_resources lst =
  List.fold_left
    (fun acc resource ->
      let name = resource |> member "name" |> to_string in
      let amount = resource |> member "amount" |> to_int in
      let new_resource = new_resource name amount in
      new_resource :: acc)
    [] lst

(** [iter_events json] is the [(string * resource list) list] extracted
    from the json list [json]. *)
let iter_events json =
  List.fold_left
    (fun acc event ->
      let text = event |> member "text" |> to_string in
      let resource_lst =
        event |> member "resource" |> to_list |> iter_resources
      in
      let damage = event |> member "defense" |> to_int in
      (text, resource_lst, damage) :: acc)
    [] json

(** [load_events] extracts the lists of easy, medium and hard events
    from the json file provided. *)
let load_events =
  let events_of_json difficulty =
    "events_init.json" |> Yojson.Basic.from_file |> member difficulty
    |> to_list |> iter_events
  in
  [
    ("easy", events_of_json "easy");
    ("medium", events_of_json "medium");
    ("hard", events_of_json "hard");
  ]

(** [merge_stockpile s1 s2] is [s2] after adding or subtracting
    resources from [s1]. *)
let rec merge_stockpile stockpile = function
  | [] -> stockpile
  | h :: t ->
      let resource_name = resource_name h in
      let resource_amount = resource_amount h in
      let filter_resource (name, value) =
        if name = resource_name then (name, value + resource_amount)
        else (name, value)
      in
      merge_stockpile (List.map filter_resource stockpile) t


(** [update_cells arr def] updates [cells] by decreasing defense levels
    of all building by [defense_change] and removes buildings with zero
    defense. *)
let update_cells cells defense_change =
  Array.iteri
    (fun i row ->
      Array.iteri
        (fun j cell ->
          match cell with
          | Building building ->
              if building.defense > defense_change then
                cells.(i).(j) <-
                  Building (decrease_defense building defense_change)
              else cells.(i).(j) <- None
          | _ -> ())
        row)
    cells

(** [generate_event st] is the [text] displayed in the event, the updated 
    [stockpile], and the updated [cells] if any buildings are demolished. *)
let generate_event state =
  let events = load_events in
  let level = List.assoc "money" state.stockpile in
  let difficulty =
    if level < 1000 then "easy"
    else if level < 2000 then "medium"
    else "hard"
  in
  let event_lst = List.assoc difficulty events in
  let f, s, t =
    List.nth event_lst (Random.int (List.length event_lst))
  in
  (f, merge_stockpile state.stockpile s, t)

(** [minus_cost stockpile cost] is [None] if [cost] exceeds quantity stored in
    [stockpile], or [Some s] where [s] is the updated [stockpile] after 
    subtracting [cost] in [stockpile]. *)
let minus_cost stockpile cost : (string * int) list option =
  let is_sufficient =
    List.fold_left
      (fun acc resource ->
        if resource_name resource = "money" && resource_amount resource >= cost
        then false && acc
        else true && acc)
      true stockpile
    |> not
  in
  match is_sufficient with
  | false -> None
  | true ->
      Some
        (List.fold_left
           (fun acc r ->
             let name = resource_name r in
             let amount = resource_amount r in
             if name = "money" then (name, amount - cost) :: acc
             else r :: acc)
           [] stockpile)

(** [enough_workforce st bld] is whether the availabel workforce in [st] is 
    enough for [bld]. *)
let enough_workforce state bld = 
  bld.population_dependency <= state.unemployed 

let place_building state name x y =
  let building =
    List.find (fun building -> building.name = name) state.buildings
  in
  let cost = building.cost in
  match minus_cost state.stockpile cost with
  | None -> ()
  | Some stockpile ->
    if enough_workforce state building then begin
      place_cell state
        (Building
           (List.find
              (fun building -> building.name = name)
              state.buildings))
        x y;
      state.stockpile <- stockpile;
      state.unemployed <- state.unemployed - building.population_dependency
      + building.housing;
      state.population <- state.population + building.housing;
    end

let next_state state =
  if not state.is_paused then begin
    update_is_game_over state;
    update_game_over_text state;
    if not state.is_game_over then begin
      state.tick <- state.tick + 1;
      update_population state;
      state.stockpile <-
        update_stockpile (available_buildings state) state.stockpile;
      update_food state;
      (*update_housing state;*)
      update_starvation_counter state;
      update_deficit_counter state;
      update_is_out_of_time state;
      update_is_in_starvation state;
      update_is_in_deficit state;
      if Random.int 15 = 0 then (
        let t, s, c = generate_event state in
        state.text <- t;
        state.stockpile <- s;
        update_cells state.cells c)
    end
  end
