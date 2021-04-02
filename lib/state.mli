(* * The type [cell] represents the most basic unit in construction that
   holds either a building, a road, or nothing. *)
type cell =
  | Building of Buildings.building
  | Road_t of Buildings.road
  | None

(** The type [state] records condition of the game at a certain instance
    of time. *)
type state

(** The type [stockpile] is a resource list that coresponds with the
    amount of resources the play has. *)
type stockpile

(** [new_state canvas_width canvas_height map_length cell_width cell_height]
    initializes a new [state]. Requires: *)
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

(** [next_state state update] is the new state by updating the existing
    [state ] with [update]. *)
val next_state : state -> state
