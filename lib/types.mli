(** The type [canvas_config] records the width and height of the HTML
    canvas. *)
type canvas_config

(** The type [map_config] records the width and height of the game map. *)
type map_config

(** The type [cell_config] records the width and height of a cell, as
    well as its fillstyle and the type of game structure it contains. *)
type cell_config

(** The type [cell] records game structure of a cell present in the game
    map. *)
type cell

(** The type [gui_config] records the [canvas_config], [map_config], and
    [cell_config]. *)
type gui_config

(** The type [game_structure] represents the most basic unit in
    construction that holds either a building, a road, or nothing. *)
type game_structure

(** The type of buildings. *)
type building

(** The type of roads. *)
type road

(** The type of resources consumed and produced by camel city. *)
type resource

(** The type of camel residents. *)
type camel

(** The type [stockpile] records amounts of resources at a certain game
    state. *)
type stockpile

(** The type [game_state] records condition of the game at a certain
    instance of time. *)
type game_state

(** [new_config] initializes a new [gui_config]. Requires: *)
val new_config :
  float -> float -> int -> int -> float -> float -> string -> gui_config

val house : building

val oats_plantation : building

val power_plant : building

val mine : building

val barrack : building

val oat : resource

val electricity : resource

val iron : resource

val money : resource

(** [creat_building conf build x_index y_index] updates [gui_config] by
    placing [building] build in location ([x_index],
    [y_index]). *)
val create_building : gui_config -> building -> int -> int -> unit

(** [collect_tax lst stock] is the updated stockpile after collecting taxes 
    from existing buildings. *)
val collect_tax : gui_config -> stockpile -> stockpile

val canvas_width : gui_config -> float

val canvas_height : gui_config -> float

val map_width : gui_config -> int

val map_height : gui_config -> int

val cell_width : gui_config -> float

val cell_height : gui_config -> float

val cell_fill_style : gui_config -> string
