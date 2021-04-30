(** [resource] is the type of resources consumed and produced by camel
    city. *)
type resource = string * int

(** [road] is a type of basic construction unit in the map. *)
type road

(** [building] is a type of basic construction unit in the map that is
    able to produce resources, tax, and some requires resources as
    input. Need money to purchase. *)
type building

(** [camel] is the type of camel residents that consume oats and
    supports operations of buildings. *)
type camel

(** [building_name building] is the name of building [building]. *)
val building_name : building -> string

(** [new_resource string int] makes a new resource [resource].*)
val new_resource : string -> int -> resource

(** [resource_name resource] is the name of resource [resource]. *)
val resource_name : resource -> string

(** [resource_amount resource] is the amount of resource [resource]. *)
val resource_amount : resource -> int

(** [resource_dependency building name] is the quantity of [name]
    dependency of [building]. *)
val resource_dependency : building -> resource list

(** [new_building name cost maint out_a out_n inc defense res_a res_n]
    is a new building with the parameters provided. *)
val new_building :
  string ->
  int ->
  int ->
  resource ->
  int ->
  int ->
  resource list ->
  building

(** [cost building] is the cost of [building].*)
val cost : building -> int

(** [new_road cost x y ] is a road of cost [cost] and location [(x, y)]. *)
val new_road : int -> int -> int -> road

(** [cost_rd road] is the cost of [road].*)
val cost_rd : road -> int

(** [output building] is the output of [building]. *)
val output : building -> resource

(** [income building] is the [income] output of [building]. *)
val income : building -> int

(** [maintenance building] is the [maintenance] cost of [building]. *)
val maintenance : building -> int

(** [defense building] is the [defense] level of [building]. *)
val defense : building -> int
