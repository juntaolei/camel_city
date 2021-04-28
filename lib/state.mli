open Buildings

(* * The type [cell] represents the most basic unit in construction that
   holds either a building, a road, or nothing. *)
type cell =
  | Building of Buildings.building
  | Road of Buildings.road
  | None

(** The type [state] records condition of the game at a certain instance
    of time. *)
type state

(** The type [stockpile] is a resource list that coresponds with the
    amount of resources the play has. *)
type stockpile = resource list

(** The type [update] should contain the necessary fields that can be
    updated. This should be types like stockpile. The game cells does
    not need to be included as cells can be mutated. *)
(* type update *)

val current_selected : state -> int

(** [select_building state i] is the current selected building from the
    building selection pane. *)
val select_building : state -> int -> unit

(** [selected_building state] is if a building is currently selected
    from the building selection pane. *)
val selected_building : state -> bool

(** [new_state canvas_width canvas_height map_length cell_width cell_height]
    initializes a new [state]. Requires: *)
val new_state :
  string ->
  ?stockpile:stockpile ->
  ?tick:int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  state

(** [canvas_size state] is the canvas size of the HTML Canvas as defined
    in [state]. *)
val canvas_size : state -> int * int

(** [map_length state] is the map length of the game map as defined in
    [state]. *)
val map_length : state -> int

(** [cell_size state] is the cell size of the game cell as defined in
    [state]. *)
val cell_size : state -> int * int

(** [population state] is the population of [state]. *)
val population : state -> int

(** [cells state] is a two dimensional array of cells as defined in
    [state]. *)
val cells : state -> cell array array

(** [str_of_cell cel] is the string representation of [cel]. *)
val str_of_cell : cell -> string

(** [stockpile state] is the stockpile of [state]. *)
val stockpile : state -> stockpile

val buildings : state -> building list

(** [next_state state update] is the new state by updating the existing
    [state ] with [update]. *)
val next_state : state -> state

val iter_buildings :
  building list -> Yojson.Basic.t list -> building list

(** [from_json file] is the state read from the file with name [file].
    [file] must be a valid file name ending in [.json]. *)
val from_file : string -> state

(** [save_state st] saves the state [st] into a json file in the same
    directory. If the file already exists, contents will be overwritten. *)
val save_state : state -> string

val place_cell : state -> cell -> int -> int -> unit
