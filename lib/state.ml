open Buildings

type stockpile = resource list

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
  | Road_t of road
  | None

type state = {
  tick : int;
  canvas_size : int * int;
  map_length : int;
  cell_size : int * int;
  cells : cell array array;
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
(** [resource_sufficiency_check_helper] checks if there is enough
    resources in the stockpile for the building to operate*)

let canvas_size state = state.canvas_size

let canvas_width state = fst state.canvas_size

let canvas_height state = snd state.canvas_size

let map_length state = state.map_length

let cell_size state = state.cell_size

let cell_width state = fst state.cell_size

let cell_height state = snd state.cell_size

let cells state = state.cells

let tick state = state.tick

let get_buildings state = state.buildings

let new_state
    ?(stockpile = [])
    ?(buildings = [])
    ?(tick = 1)
    (canvas_width : int)
    (canvas_height : int)
    (map_length : int)
    (cell_width : int)
    (cell_height : int) =
  {
    tick;
    canvas_size = (canvas_width, canvas_height);
    map_length;
    cell_size = (cell_width, cell_height);
    cells = build_cell_lst map_length map_length;
    stockpile;
    buildings;
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
        new_state ~tick:(state.tick + 1) (canvas_width state)
          (canvas_height state) (map_length state) (cell_width state)
          (cell_height state) ~stockpile:new_resources
          ~buildings:(get_buildings state)
      in
      next_state update_state
    else next_state state
  else next_state state

(* order of update? option 1: geographic location (recursion through the
   cell list list) option 2: oat_plantation-> power_plant -> mine (so
   that the resource produced can be used as inputs for other buildlings
   immediately in the same round of update) maybe consider this for MS2? *)
(* let update_stockpile stockpile state = failwith "Unimplemented" *)
(* pile |> update conf |> failwith "unimplemented" *)
