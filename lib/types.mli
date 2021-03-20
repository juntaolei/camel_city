type gui_config

val new_config :
  float -> float -> int -> int -> float -> float -> string -> gui_config

val canvas_width : gui_config -> float

val canvas_height : gui_config -> float

val map_width : gui_config -> int

val map_height : gui_config -> int

val cell_width : gui_config -> float

val cell_height : gui_config -> float

val cell_fill_style : gui_config -> string
