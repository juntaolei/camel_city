(** [building] is a type of basic construction unit in the map that is
    able to produce resources, tax, and some requires resources as
    input. Need money to purchase. *)
type building = {
  name : string;
  cost : int;
  maintenance : int;
  output : string * int;
  income : int;
  defense : int;
  resource_dependency : (string * int) list;
  (*happiness : int;*)
  population_dependency : int;
  housing : int;
  (*entertainment : int;*)
  is_final_building : bool;
}

type road = int * int

(** [new_building name cost maint out_a out_n inc defense res_a res_n]
    is a new building with the parameters provided. *)
val new_building :
  string ->
  int ->
  int ->
  string * int ->
  int ->
  int ->
  (string * int) list ->
  (*int ->*)
  int ->
  int ->
  (*int ->*)
  bool ->
  building

val new_road : int -> int -> road

(** [new_resource string int] makes a new resource [resource].*)
val new_resource : string -> int -> string * int

(** [resource_name resource] is the name of resource [resource]. *)
val resource_name : string * int -> string

(** [resource_amount resource] is the amount of resource [resource]. *)
val resource_amount : string * int -> int

(** [dec_defense building i] is the [building] with defense level
    decreased by [i]. *)
val decrease_defense : building -> int -> building
