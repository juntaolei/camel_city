(** The type [update] should contain the necessary fields that can be
    updated. This should be types like stockpile. The game cells does
    not need to be included as cells can be mutated. *)
type update

(** [next_state state update] is the new state by updating the existing
    [state ] with [update]. *)
val next_state : Types.state -> update -> Types.state
(** This is the only function that should be exposed outside of this
    module. Implementing this function requires merging the values
    inside the update state to the existing game state to create a new
    state. For further optimization, change the type signature to the
    one below. *)
(* val next_state : Types.state ref -> update -> unit *)
