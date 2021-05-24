open Buildings

(* * The type [cell] represents the most basic unit in construction that
   holds either a building, a road, or nothing. *)
type cell =
  | Building of Buildings.building
  | Road of Buildings.road
  | None

(** The type [state] records condition of the game at a certain instance
    of time. *)
type state = {
  mutable tick : int;
  mutable interval_time : float;
  mutable last_updated : float;
  mutable text : string;
  mutable canvas_size : int * int;
  mutable map_length : int;
  mutable cell_size : int * int;
  mutable buildings : building list;
  mutable cells : cell array array;
  mutable selected_cell : int;
  mutable housing_capacity : int;
  mutable military_strength : int;
  mutable population : int;
  mutable food : int;
  mutable money : int;
  mutable deficit_counter : int;
  mutable starvation_counter : int;
  mutable revolt_counter : int;
  mutable happiness : float;
  mutable is_paused : bool;
  mutable is_game_over : bool;
  mutable is_in_deficit : bool;
  mutable is_in_starvation : bool;
  mutable is_out_of_time : bool;
  mutable is_in_revolt : bool;
  mutable is_defeated : bool;
  mutable is_final_building_placed : bool;
  mutable game_over_message : string;
  mutable stockpile : (string * int) list;
}

(** [new_state canvas_width canvas_height map_length cell_width cell_height]
    initializes a new [state]. Requires: *)
val new_state :
  ?tick:int ->
  ?housing_capacity:int ->
  ?military_strength:int ->
  ?population:int ->
  ?food:int ->
  ?money:int ->
  ?deficit_counter:int ->
  ?starvation_counter:int ->
  ?revolt_counter:int ->
  ?happiness:float ->
  ?is_paused:bool ->
  ?is_game_over:bool ->
  ?is_in_deficit:bool ->
  ?is_in_starvation:bool ->
  ?is_out_of_time:bool ->
  ?is_in_revolt:bool ->
  ?is_defeated:bool ->
  ?is_final_building_placed:bool ->
  int ->
  int ->
  int ->
  int ->
  int ->
  state

(** [select_cell state i] is the current selected building from the
    building selection pane. *)
val select_cell : state -> int -> unit

(** [is_selected state] is if a building is currently selected from the
    building selection pane. *)
val is_selected : state -> bool

(** [str_of_cell cel] is the string representation of [cel]. *)
val str_of_cell : cell -> string

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

val place_building : state -> string -> int -> int -> unit
