type road = {
  cost : int;
  x_cord : int;
  y_cord : int;
}

type resource = {
  amount : int;
  name : string;
}

type building = {
  name : string;
  cost : int;
  maintenance : int;
  output : resource;
  tax : int;
  defense : int;
  building_dependency : building list;
  resource_dependency : resource list;
}

type game_structure =
  | Building of building
  | Road of road
  | None

type cell_config = {
  game_structure : game_structure;
  width : float;
  height : float;
  fill_style : string;
}

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

type camel = { food : int }

type stockpile = { resources : resource list }

type game_state = { gui : gui_config }

let house =
  {
    name = "house";
    cost = 0;
    maintenance = 0;
    output = { amount = 0; name = "" };
    tax = 0;
    defense = 0;
    building_dependency = [];
    resource_dependency = [];
  }

let oat = { amount = 0; name = "oat" }

let oats_plantation =
  {
    name = "oats planation";
    cost = 0;
    maintenance = 0;
    output = oat;
    tax = 0;
    defense = 0;
    building_dependency = [];
    resource_dependency = [];
  }

let electricity = { amount = 0; name = "electricity" }

let power_plant =
  {
    name = "power plant";
    cost = 0;
    maintenance = 0;
    output = electricity;
    tax = 0;
    defense = 0;
    building_dependency = [];
    resource_dependency = [];
  }

let barrack =
  {
    name = "power plant";
    cost = 0;
    maintenance = 0;
    output = { amount = 0; name = "" };
    tax = 0;
    defense = 0;
    building_dependency = [];
    resource_dependency = [];
  }

let new_config x y m_x m_y c_x c_y fill_style =
  {
    canvas_config = { width = x; height = y };
    map_config = { width = m_x; height = m_y };
    cell_config =
      { game_structure = None; width = c_x; height = c_y; fill_style };
  }

let canvas_width config = config.canvas_config.width

let canvas_height config = config.canvas_config.height

let map_width config = config.map_config.width

let map_height config = config.map_config.height

let cell_width config = config.cell_config.width

let cell_height config = config.cell_config.height

let cell_fill_style config = config.cell_config.fill_style
