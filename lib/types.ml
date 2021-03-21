type canvas_config = {
  width : float;
  height : float;
}

type map_config = {
  width : int;
  height : int;
}

type gui_config = {
  canvas_config : canvas_config;
  map_config : map_config;
  cell_config : cell_config;
}

type cell_config = {
  (** tile is commented out because other existing functions are not fixed for the updated cell_config type. *)
  (* tile : tile; *)
  width : float;
  height : float;
  fill_style : string;
}

type tile =
  | Building of building
  | Road of road
  | None

type building = {
  name : string;
  cost : int;
  maintenance : int;
  output : int;
  defense : int;
  building_dependency : building list;
  resource_dependency : resource list;
}

type road = {
  cost : int;
  x_cord : int;
  y_cord : int;
}

type resource = {
  amount : int;
  name : string;
}

type camel = {
  food : int;
}

type stockpile = {
  resources : resource list;
}

type game_state = {
  gui : gui_config;
}

type game_state = { config : gui_config }

let house = failwith "unimplemented"

let oats_planation = failwith "unimplemented"

let oat = failwith "unimplemented"

let power_plant = failwith "unimplemented"

let electricity = failwith "unimplemented"

let barrack = failwith "unimplemented"

let new_config x y m_x m_y c_x c_y fill_style =
  {
    canvas_config = { width = x; height = y };
    map_config = { width = m_x; height = m_y };
    cell_config = { width = c_x; height = c_y; fill_style };
  }

let canvas_width config = config.canvas_config.width

let canvas_height config = config.canvas_config.height

let map_width config = config.map_config.width

let map_height config = config.map_config.height

let cell_width config = config.cell_config.width

let cell_height config = config.cell_config.height

let cell_fill_style config = config.cell_config.fill_style
