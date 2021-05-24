open Buildings
open Yojson.Basic.Util

(** [default_stockpile] is the stockpile on any fresh game sessions. *)
let default_stockpile =
  [
    ("money", 200);
    ("electricity", 0);
    ("food", 0);
    ("iron", 0);
    ("coal", 0);
  ]

let deficit_limit = 180

let starvation_limit = 120

let revolt_limit = 300

let time_limit = 7200

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
  mutable military_strength : int;
  mutable population : int;
  mutable food : int;
  mutable money : int;
  mutable deficit_counter : int;
  mutable starvation_counter : int;
  mutable revolt_counter : int;
  mutable happiness : float;
  mutable is_paused : bool;
  mutable is_game_over : bool;
  mutable is_in_deficit : bool;
  mutable is_in_starvation : bool;
  mutable is_out_of_time : bool;
  mutable is_in_revolt : bool;
  mutable is_defeated : bool;
  mutable is_final_building_placed : bool;
  mutable game_over_message : string;
  mutable stockpile : (string * int) list;
}

(** [build_cell_lst width height] is an two dimensional array with
    [width] and [height]. *)
let build_cell_lst width height = Array.make_matrix width height None

let select_cell state i = state.selected_cell <- i

let is_selected state = state.selected_cell >= 0

let place_cell state cell x y = state.cells.(x).(y) <- cell

let str_of_cell = function
  | None -> ""
  | Road _ -> "road"
  | Building b -> b.name

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
      let happiness = int_of_member "happiness" in
      let population_dependency =
        int_of_member "population_dependency"
      in
      let housing = int_of_member "housing" in
      let entertainment = int_of_member "entertainment" in
      let is_final_building =
        building |> member "is_final_building" |> to_bool
      in
      new_building name cost maintenance output tax defense
        resource_dependency happiness population_dependency housing
        entertainment is_final_building
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
    ?(military_strength = 0)
    ?(population = 0)
    ?(food = 0)
    ?(money = 0)
    ?(deficit_counter = 0)
    ?(starvation_counter = 0)
    ?(revolt_counter = 0)
    ?(happiness = 0.)
    ?(is_paused = true)
    ?(is_game_over = false)
    ?(is_in_deficit = false)
    ?(is_in_starvation = false)
    ?(is_out_of_time = false)
    ?(is_in_revolt = false)
    ?(is_defeated = false)
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
    military_strength;
    population;
    food;
    money;
    happiness;
    deficit_counter;
    starvation_counter;
    revolt_counter;
    is_paused;
    is_game_over;
    is_in_deficit;
    is_in_starvation;
    is_out_of_time;
    is_in_revolt;
    is_defeated;
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

let update_housing state =
  let housing_capacity =
    List.fold_left
      (fun acc building -> building.housing + acc)
      0
      (available_buildings state)
  in
  state.housing_capacity <- housing_capacity

let update_defense state =
  let happiness_effect =
    if state.happiness < 30. then -20
    else if state.happiness < 40. then -10
    else if state.happiness < 60. then 10
    else 20
  in
  let defense_score =
    List.fold_left
      (fun acc building -> building.defense + acc)
      0
      (available_buildings state)
  in
  state.military_strength <- defense_score + happiness_effect

let update_food state =
  state.food <-
    List.find (fun (name, _) -> name = "food") state.stockpile
    |> resource_amount

let consume_food state = state.food <- state.food - state.population

let update_happiness state =
  let buildings = state |> available_buildings in
  let entertainment_happiness =
    List.fold_left
      (fun acc (building : building) -> building.happiness + acc)
      0 buildings
    |> float_of_int
  in
  let homelessness_score =
    (let homeless_camels = state.population - state.housing_capacity in
     if state.housing_capacity < 1 then 0 else homeless_camels)
    |> float_of_int
  in
  let population_score =
    state.population |> float_of_int |> ( *. ) 0.1
  in
  state.happiness <-
    50.0 +. entertainment_happiness +. population_score
    -. homelessness_score

let update_population state =
  if state.tick mod 60 = 0 then state.population <- state.population + 5

let update_starvation_counter state =
  if state.food < 0 then
    state.starvation_counter <- state.starvation_counter + 1
  else if state.starvation_counter > 0 then
    state.starvation_counter <- state.starvation_counter - 1

let update_deficit_counter state =
  if state.money < 0 then
    state.deficit_counter <- state.deficit_counter + 1
  else if state.deficit_counter > 0 then
    state.deficit_counter <- state.deficit_counter - 1

let update_revolt_counter state =
  if state.happiness < 30. then
    state.revolt_counter <- state.revolt_counter + 1
  else if state.revolt_counter > 0 then
    state.revolt_counter <- state.revolt_counter - 1

let update_is_out_of_time state =
  state.is_out_of_time <- state.tick < time_limit

let update_is_in_starvation state =
  state.is_in_starvation <- state.starvation_counter < starvation_limit

let update_is_in_deficit state =
  state.is_in_deficit <- state.deficit_counter < deficit_limit

let update_is_in_revolt state =
  state.is_in_revolt <- state.revolt_counter < revolt_limit

let update_is_game_over state =
  if
    state.is_in_deficit || state.is_in_starvation
    || state.is_out_of_time || state.is_in_revolt || state.is_defeated
    || state.is_final_building_placed
  then state.is_game_over <- true

let update_game_over_text state =
  let message =
    if not state.is_in_deficit then "Your city is bankrupted."
    else if not state.is_in_starvation then "Your city is starved."
    else if not state.is_out_of_time then "You ran out of time."
    else if not state.is_in_revolt then
      "Your city is destroyed by revolts."
    else if not state.is_defeated then "You have been defeated."
    else if not state.is_final_building_placed then "You won."
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
  let float_of_member name = json |> member name |> to_float in
  let bool_of_member name = json |> member name |> to_bool in
  let init_state =
    new_state ~tick:(int_of_member "tick")
      ~housing_capacity:(int_of_member "housing_capacity")
      ~military_strength:(int_of_member "military_strength")
      ~population:(int_of_member "population")
      ~food:(int_of_member "food") ~money:(int_of_member "money")
      ~deficit_counter:(int_of_member "deficit_counter")
      ~starvation_counter:(int_of_member "starvation_counter")
      ~revolt_counter:(int_of_member "revolt_counter")
      ~happiness:(float_of_member "happiness")
      ~is_paused:true
      ~is_game_over:(bool_of_member "is_game_over")
      ~is_in_deficit:(bool_of_member "is_in_deficit")
      ~is_in_starvation:(bool_of_member "is_in_starvation")
      ~is_out_of_time:(bool_of_member "is_out_of_time")
      ~is_in_revolt:(bool_of_member "is_in_revolt")
      ~is_defeated:(bool_of_member "is_defeated")
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
      ("military_strength", `Int state.military_strength);
      ("population", `Int state.population);
      ("food", `Int state.food);
      ("money", `Int state.money);
      ("happiness", `Float state.happiness);
      ("deficit_counter", `Int state.deficit_counter);
      ("starvation_counter", `Int state.starvation_counter);
      ("revolt_counter", `Int state.revolt_counter);
      ("is_paused", `Bool state.is_paused);
      ("is_game_over", `Bool state.is_game_over);
      ("is_in_deficit", `Bool state.is_in_deficit);
      ("is_in_starvation", `Bool state.is_in_starvation);
      ("is_out_of_time", `Bool state.is_out_of_time);
      ("is_in_revolt", `Bool state.is_in_revolt);
      ("is_defeated", `Bool state.is_defeated);
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
let merge_stockpile s1 s2 =
  (* function | [] -> stockpile | h :: t -> let resource_name =
     resource_name h in let resource_amount = resource_amount h in let
     filter_resource (name, value) = if name = resource_name then (name,
     value + resource_amount) else (name, value) in merge_stock
     (List.map filter_resource stockpile) t *)
  List.fold_left
    (fun acc resource ->
      List.map
        (fun (name, value) ->
          if name = resource_name resource then
            (name, value + resource_amount resource)
          else (name, value))
        acc)
    s2 s1

(** [update_cells arr def] updates [cells] by decreasing defense levels
    of all building by [defense_change] and removes buildings with zero
    defense. *)
let update_cells cells defense_change =
  (* for i = 0 to Array.length arr - 1 do for j = 0 to Array.length
     arr.(0) - 1 do match arr.(i).(j) with | Building b -> let curr =
     defense b in if curr > def then arr.(i).(j) <- Building
     (decrease_defense b def) else arr.(i).(j) <- None | _ -> () done
     done *)
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

let minus_cost stockpile cost : (string * int) list option =
  let is_sufficient =
    List.fold_left
      (fun acc resource ->
        if
          resource_name resource = "money"
          && resource_amount resource >= cost
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

let place_building state name x y =
  let building =
    List.find (fun building -> building.name = name) state.buildings
  in
  let cost = building.cost in
  match minus_cost state.stockpile cost with
  | None -> ()
  | Some stockpile ->
      place_cell state
        (Building
           (List.find
              (fun building -> building.name = name)
              state.buildings))
        x y;
      state.stockpile <- stockpile

let next_state state =
  update_is_game_over state;
  update_game_over_text state;
  if not state.is_game_over then begin
    state.tick <- state.tick + 1;
    update_population state;
    state.stockpile <-
      update_stockpile (available_buildings state) state.stockpile;
    update_food state;
    consume_food state;
    update_housing state;
    update_defense state;
    update_happiness state;
    update_starvation_counter state;
    update_deficit_counter state;
    update_revolt_counter state;
    update_is_out_of_time state;
    update_is_in_starvation state;
    update_is_in_deficit state;
    update_is_in_revolt state;
    if Random.int 100 = 0 then (
      let t, s, c = generate_event state in
      state.text <- t;
      state.stockpile <- s;
      update_cells state.cells c)
  end
