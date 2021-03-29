open Buildings

type gui_config = {
  canvas_size : int * int;
  map_length : int;
  cell_size : int * int;
}

type cell =
  | Building of building
  | Road_t of road
  | None

type state = {
  gui_config : gui_config;
  cells : cell array array;
}

(* For now: index 0 = oat, index 1 = electricity, index 2 = iron, index
   3 = money *)
type stockpile = resource list

let build_cell_lst width height = Array.make_matrix width height None

let place_cell state cell x y = state.cells.(x).(y) <- cell

let tax_amount state =
  Array.fold_left
    (fun acc row ->
      acc
      + Array.fold_left
          (fun acc ele ->
            match ele with Building b -> get_tax b + acc | _ -> acc)
          0 row)
    0 state.cells

(** [merge_stock s1 s2] is a helper function that combines resources in
    [s1] and [s2] into a single stockpile. *)
let rec merge_stock s1 s2 =
  List.map2 (fun e1 e2 -> resource_amount e1 + resource_amount e2) s1 s2

(** [update_tax pile] is the [pile] after collecting tax in the game
    [state]. *)
let update_tax state pile =
  let tax_pile =
    [
      new_resource "" 0;
      new_resource "" 0;
      new_resource "" 0;
      new_resource "money" (tax_amount state);
    ]
  in
  merge_stock pile tax_pile

(* order of update? option 1: geographic location (recursion through the
   cell list list) option 2: oat_plantation-> power_plant -> mine (so
   that the resource produced can be used as inputs for other buildlings
   immediately in the same round of update) maybe consider this for MS2? *)
let update_stockpile pile conf = failwith "Unimplemented"
(* pile |> update conf |> failwith "unimplemented" *)

let new_state
    canvas_width
    canvas_height
    map_length
    cell_width
    cell_height =
  let gui_config =
    {
      canvas_size = (canvas_width, canvas_height);
      map_length;
      cell_size = (cell_width, cell_height);
    }
  in
  { gui_config; cells = build_cell_lst map_length map_length }

let canvas_size state = state.gui_config.canvas_size

let map_length state = state.gui_config.map_length

let cell_size state = state.gui_config.cell_size

let cells state = state.cells
