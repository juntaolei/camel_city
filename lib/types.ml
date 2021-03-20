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

type gui_config = {
  canvas_config : canvas_config;
  map_config : map_config;
  cell_config : cell_config;
}

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
