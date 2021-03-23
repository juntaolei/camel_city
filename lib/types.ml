open Building

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
  | Road of road
  | None

type gui_config = {
  canvas_config : canvas_config;
  map_config : map_config;
  cell_config : cell_config;
  cell : cell list list;
}

type game_state = { gui : gui_config }

type stockpile = { 
  oat_stock : oat;
  electricity_stock : electricity;
  iron_stock : iron;
  money_stock : money
}

let place_cell conf cell x_coord y_coord =
  List.mapi (fun i x -> if i = x_coord then 
    List.mapi (fun i y -> if i = y_coord then cell else y) else x) conf.cell

(* helper functions *)
(* summing tax for a cell list*)
let rec sum_tax = function
| [] -> 0
| h::t -> match h with
          | Building of building ->
            building.tax + sum_tax t
          | _ -> sum_tax t

let rec tax_amount = function
| [] -> 0
| h :: t -> (sum_tax h) + tax_amount t

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