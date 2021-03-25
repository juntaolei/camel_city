open Buildings

type canvas_config = {
  width : float;
  height : float;
}

type map_config = {
  width : int;
  height : int;
}

type cell_config = {
  width : float;
  height : float;
  fill_style : string;
}

type cell =
  | Building of building
  | Road_t of road
  | None

type gui_config = {
  canvas_config : canvas_config;
  map_config : map_config;
  cell_config : cell_config;
  cell : cell list list;
}

type game_state = { gui : gui_config }

(* It would be easier to use a hash table for lookup. *)
type stockpile = { lst : resource list }

(* For now: index 0 = oat, index 1 = electricity, index 2 = iron, index
   3 = money*)
(* type stockpile = (string, int) Hashtbl.t *)

(* helper function *)
let rec place_row acc_r y cell = function
  | [] -> acc_r
  | h :: t ->
      place_row
        ((if List.length acc_r = y then cell else h) :: acc_r)
        y cell t

let place_cell conf cell x_coord y_coord =
  let rec place_col acc_c = function
    | [] -> acc_c
    | h :: t ->
        place_col
          ((if List.length acc_c = x_coord then
            place_row [] y_coord cell h
           else h)
           :: acc_c)
          t
  in
  {
    canvas_config = conf.canvas_config;
    map_config = conf.map_config;
    cell_config = conf.cell_config;
    cell = List.rev (place_col [] conf.cell);
  }

(* helper function *)
let rec sum_row_tax = function
  | [] -> 0
  | h :: t -> (
      match h with
      | Building building -> get_tax building + sum_row_tax t
      | _ -> sum_row_tax t)

let rec tax_amount conf =
  let rec tax_amount_lst = function
    | [] -> 0
    | h :: t -> sum_row_tax h + tax_amount_lst t
  in
  tax_amount_lst conf.cell

(** [merge_stock s1 s2 \[\]] is a helper function that combines
    resources in [s1] and [s2] into a single stockpile. *)
let rec merge_stock s1 s2 acc =
  match List.length acc with
  | 4 -> acc
  | _ ->
      merge_stock s1 s2
        (new_resource (List.nth s1 (3 - List.length acc)).name
           (List.nth s1 (3 - List.length acc)).amount
         + (List.nth s2 (3 - List.length acc)).amount
         :: acc)

(** The following merges s1 into s2. *)

(** let merge_stockpile s1 s2 = Hashtbl.iter (fun k v -> Hashtbl.replace
    s2 k (Hashtbl.find s1 k + v)) s2 *)

(* helper function [update_tax pile] is the [pile] after collecting tax.*)
let update_tax conf pile =
  let tax_pile =
    [
      new_resource 0 "";
      new_resource 0 "";
      new_resource 0 "";
      new_resource (tax_amount conf) "money";
    ]
  in
  merge_stock pile tax_pile []

(* order of update: geographic location or collect tax -> oat_plantation
   -> power_plant -> mine *)
let update_stockpile pile conf =
  pile |> update conf |> failwith "unimplemented"

let new_config x y m_x m_y c_x c_y fill_style =
  {
    canvas_config = { width = x; height = y };
    map_config = { width = m_x; height = m_y };
    cell_config = { width = c_x; height = c_y; fill_style };
    cell = [];
  }

let canvas_width config = config.canvas_config.width

let canvas_height config = config.canvas_config.height

let map_width config = config.map_config.width

let map_height config = config.map_config.height

let cell_width config = config.cell_config.width

let cell_height config = config.cell_config.height

let cell_fill_style config = config.cell_config.fill_style
