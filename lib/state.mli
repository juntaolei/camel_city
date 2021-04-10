(** The type [cell] represents the most basic unit in construction that
    holds either a building, a road, or nothing. *)
type cell =
  | Building of Buildings.building
  | Road_t of Buildings.road
  | None

(** The type [state] records condition of the game at a certain instance
    of time. *)
type state

(** The type [stockpile] records amounts of resources at a certain game
    state. *)
type stockpile

(** The type [update] should contain the necessary fields that can be
    updated. This should be types like stockpile. The game cells does
    not need to be included as cells can be mutated. *)
type update

(** [select_building state i] is the current selected building from the
    building selection pane. *)
val select_building : state -> int -> unit

(** [selected_building state] is if a building is currently selected
    from the building selection pane. *)
val selected_building : state -> bool

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
val next_state : state -> update -> state

(** This is the only function that should be exposed outside of this
    module. Implementing this function requires merging the values
    inside the update state to the existing game state to create a new
    state. For further optimization, change the type signature to the
    one below. *)
(* val next_state : Types.state ref -> update -> unit *)
