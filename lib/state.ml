open Buildings

(* For now: index 0 = oat, index 1 = electricity, index 2 = iron, index
   3 = money *)
type stockpile = resource list

type update = { stockpile : stockpile }

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
(* let update_stockpile stockpile state = failwith "Unimplemented" *)
(* pile |> update conf |> failwith "unimplemented" *)

let new_state
    canvas_width
    canvas_height
    map_length
    cell_width
    cell_height =
  {
    tick = 1;
    canvas_size = (canvas_width, canvas_height);
    map_length;
    cell_size = (cell_width, cell_height);
    cells = build_cell_lst map_length map_length;
    stockpile = [];
    buildings = [];
    selected_building = -1;
  }

let canvas_size state = state.canvas_size

let map_length state = state.map_length

let cell_size state = state.cell_size

let cells state = state.cells

let next_state state (update : update) =
  {
    state with
    stockpile = merge_stockpile state.stockpile update.stockpile;
  }
