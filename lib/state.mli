open Buildings

(** The type [stockpile] is a resource list that coresponds with the
    amount of resources the play has. *)
type stockpile = resource list

(* * The type [cell] represents the most basic unit in construction that
   holds either a building, a road, or nothing. *)
type cell =
  | Building of Buildings.building
  | Road of Buildings.road
  | None

(** The type [state] records condition of the game at a certain instance
    of time. *)
type state

(** [new_state canvas_width canvas_height map_length cell_width cell_height]
    initializes a new [state]. Requires: *)
val new_state :
  ?stockpile:stockpile ->
  ?tick:int ->
  string ->
  int ->
  int ->
  int ->
  int ->
  int ->
  state

(** [current_selected state] is the ID of the currently selected
    building from the building selection pane. *)
val current_selected : state -> int

(** [select_building state i] is the current selected building from the
    building selection pane. *)
val select_building : state -> int -> unit

(** [selected_building state] is if a building is currently selected
    from the building selection pane. *)
val selected_building : state -> bool

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

(** [tick state] is the current tick of the [state]. *)
val tick : state -> int

(** [stockpile state] is the stockpile of [state]. *)
val stockpile : state -> stockpile

(** [text state] is the text of [state]. *)
val text : state -> string

(** [str_of_cell cel] is the string representation of [cel]. *)
val str_of_cell : cell -> string

(** [minus_cost pile cost] is [None] if resources in pile cannot afford [cost], 
    or [Some p] where p is the updated stockpile after deducting costs from 
    [pile]. *)
val minus_cost : stockpile -> int -> stockpile option

(** [update_stock state pile] updates the stockpile of [state] with [pile]. *)
val update_stock : state -> stockpile -> unit

(** [buildings state] is the available building list of [state]. *)
val buildings : state -> building list

(** [place_cell state cell x y] places a [cell] inside [state] by its
    [x] and [y] coordinates. *)
val place_cell : state -> cell -> int -> int -> unit

(** [next_state state update] is the new state by updating the existing
    [state ] with [update]. *)
val next_state : state -> unit

(** [from_json string] is the state read from the string with name
    [string]. [string] must be a valid JSON. *)
val from_string : string -> state

(** [save_state st] saves the state [st] into a json file in the same
    directory. If the file already exists, contents will be overwritten. *)
val save_state : state -> string

(** [generate_event st] is the text displayed, corresponding stockpile
    addition or subtraction, and damage in defense, based on conditions 
    of state [st]. *)
val generate_event : state -> (string * stockpile * int)