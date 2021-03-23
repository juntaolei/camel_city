(** The type of resources consumed and produced by camel city. *)
type resource

(** The type of roads. *)
type road

(** The type of buildings. *)
type building

(** The type of camel residents. *)
type camel

val house : building

val oats_plantation : building

val power_plant : building

val mine : building

val barrack : building

val oat : resource

val electricity : resource

val iron : resource

val money : resource

(*
(** [place_cell conf cel x_index y_index] updates [gui_config] conf by
    placing [cell] cel in location ([x_index], [y_index]). *)
val place_cell : Types.gui_config -> Types.cell -> int -> int -> unit

(** [tax_amount conf] is the amount of tax collected from existing cells in
    a unit of time. *)
val tax_amount : Types.gui_config -> int
*)