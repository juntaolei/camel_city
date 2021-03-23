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
  employment : int;
  building_dependency : building list;
  resource_dependency : resource list;
}

type cell =
  | Building of building
  | Road of road
  | None

type cell_config = {
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
  cell : cell array array;
}

type camel = { food : int; employment : building}

type stockpile = { 
  oat_stock : oat;
  electricity_stock : electricity;
  iron_stock : iron;
  money_stock : money
}

type game_state = { gui : gui_config }

let house =
  {
    name = "house";
    cost = 0;
    maintenance = 0;
    output = { amount = 0; name = "" };
    tax = 0;
    defense = 0;
    employment = 0;
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
    employment = 0;
    building_dependency = [];
    resource_dependency = [];
  }

let electricity = { amount = 0; name = "electricity" }

let power_plant =
  {
    name = "power_plant";
    cost = 0;
    maintenance = 0;
    output = electricity;
    tax = 0;
    defense = 0;
    employment = 0;
    building_dependency = [];
    resource_dependency = [];
  }

let iron = { amount = 0; name = "iron" }

let mine =
  {
    name = "mine";
    cost = 0;
    maintenance = 0;
    output = iron;
    tax = 0;
    defense = 0;
    employment = 0;
    building_dependency = [];
    resource_dependency = [];
  }

let money = { amount = 0; name = "money" }

let barrack =
  {
    name = "power plant";
    cost = 0;
    maintenance = 0;
    output = { amount = 0; name = "" };
    tax = 0;
    defense = 0;
    employment = 0;
    building_dependency = [];
    resource_dependency = [];
  }

let create_building conf build x_coord y_coord =
  Array.set conf.cell.(x_coord) y_coord build 

(* helper func *)
let rec tax_for_row acc row = 
  match row.length with
  | 0 -> acc
  | _ -> match row.(row.length - 1) with
         | Building of building ->
            tax_for_row (acc + building.tax) (Array.sub row 0 row.length - 1)
         | _ -> tax_for_row acc (Array.sub row 0 row.length - 1)

let collect_tax conf stock =
  let rec sum_tax acc conf =
    match conf.length with
    | 0 -> acc
    | _ -> 
      sum_tax (tax_for_row acc conf.(conf.length - 1)) 
      (Array.sub conf 0 conf.length - 1)
  in 
  {
    oact_stock = stock.oat_stock;
    electricity_stock = stock.electricity_stock;
    iron_stock = stock.iron_stock;
    money_stock = {
      amount = sum_tax conf.cell stock.mone_stock.amount;
      name = "money"
      }
  }

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

let create_building building x y = failwith "Unimplemented"

let collect_tax lst = failwith "Unimplemented"
