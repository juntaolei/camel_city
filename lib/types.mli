(** The type [canvas_config] records the width and height of the HTML
    canvas. *)
type canvas_config

(** The type [map_config] records the width and height of the game map. *)
type map_config

(** The type [cell_config] records the width and height of a cell, as
    well as its fillstyle and the type of game structure it contains. *)
type cell_config

(** The type [cell] represents the most basic unit in
    construction that holds either a building, a road, or nothing. *)
type cell

(** The type [gui_config] records the [canvas_config], [map_config], and
    [cell_config]. *)
type gui_config

(** The type [game_state] records condition of the game at a certain
    instance of time. *)
type game_state

(** The type [stockpile] records amounts of resources at a certain game
    state. *)
type stockpile

(** [place_cell conf cel x_index y_index] is the updated [gui_config] conf by
    placing [cell] cel in location ([x_index], [y_index]). *)
val place_cell : gui_config -> cell -> int -> int -> gui_config

(** [tax_amount conf] is the amount of tax collected from existing cells in
    a unit of time. *)
val tax_amount : gui_config -> int

(** [update_stockpile conf pile] is the updated [pile] after collecting and
    consuming resources from buildings in [conf]. *)
val update_stockpile : gui_config -> stockpile -> stockpile

(** [new_config] initializes a new [gui_config]. Requires: *)
val new_config :
  float -> float -> int -> int -> float -> float -> string -> gui_config

val canvas_width : gui_config -> float

val canvas_height : gui_config -> float

val map_width : gui_config -> int

val map_height : gui_config -> int

val cell_width : gui_config -> float

val cell_height : gui_config -> float

val cell_fill_style : gui_config -> string
