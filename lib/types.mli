(** *)
type canvas_config

(** *)
type map_config

(** *)
type gui_config

(** *)
type cell_config

(** The type [tiles] represents the most basic unit in construction that holds
    either a building, a road, or nothing. *)
type tile =
  | Building of building
  | Road of road
  | None

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

(** The type [game_state] records condition of the game at a certain instance
    of time. *)
type game_state

(** [new_config] initializes a new [gui_config].
    Requires:  *)
val new_config :
  float -> float -> int -> int -> float -> float -> string -> gui_config

val house : building

val oats_plantation : building

val power_plant : building

val canvas_width : gui_config -> float

val canvas_height : gui_config -> float

val map_width : gui_config -> int

val map_height : gui_config -> int

val cell_width : gui_config -> float

val cell_height : gui_config -> float

val cell_fill_style : gui_config -> string
