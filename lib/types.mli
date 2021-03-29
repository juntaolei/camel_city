(** The type [cell] represents the most basic unit in construction that
    holds either a building, a road, or nothing. *)
type cell

(** The type [state] records condition of the game at a certain instance
    of time. *)
type state

(** The type [stockpile] records amounts of resources at a certain game
    state. *)
type stockpile

(** [build_cell_lst width height] is a two-dimensional array of size
    [width] by [height] with type [cell]. *)
val build_cell_lst : int -> int -> cell array array

(** [place_cell state cell x y] replaces the [cells] of the game [state]
    at indices [x] and [y] with a new [cell]. *)
val place_cell : state -> cell -> int -> int -> unit

(** [tax_amount state] is the amount of tax collected from existing
    cells in a unit of time. *)
val tax_amount : state -> int

(** [update_stockpile conf pile] is the updated [pile] after collecting
    and consuming resources from buildings in [conf]. *)
val update_stockpile : state -> stockpile -> stockpile
(** Hello Matteo this function is for you to implement. functions
    defined above might be helpful. *)

(** [new_state canvas_width canvas_height map_length cell_width cell_height]
    initializes a new [gui_config]. Requires: *)
val new_state : int -> int -> int -> int -> int -> state

(** [canvas_size state] is the canvas size of the HTML Canvas as defined
    in [state]. *)
val canvas_size : state -> int * int

(** [map_length state] is the map length of the game map as defined in
    [state]. *)
val map_length : state -> int

(** [cell_size state] is the cell size of the game cell as defined in
    [state]. *)
val cell_size : state -> int * int

(** [cells state] is a two dimensional array of cells as defined in
    [state]. *)
val cells : state -> cell array array
