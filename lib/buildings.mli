(** [resource] is the type of resources consumed and produced by camel
    city. *)
type resource

(** [road] is a type of basic construction unit in the map. *)
type road

(** [building] is a type of basic construction unit in the map that is
    able to produce resources, tax, and some requires resources as
    input. Need money to purchase. *)
type building

(** [camel] is the type of camel residents that consume oats and
    supports operations of buildings. *)
type camel

(** [oat] is a [resource] consumed by camels and produced by
    oat_plantation. *)
val oat : resource

(** [oat] is a [resource] consumed by camels. *)
val electricity : resource

(** [oat] is a [resource] consumed by mines. *)
val iron : resource

(** [money] is a [resource] collected from buildings. *)
val money : resource

(*
should we just make buildings generic? i.e. create a list of buildings, 
with each index corresponding to a type?

(** [house] is a [building] that allows camel residents, thereby
    increasing camelcity's population. *)
val house : building

(** [oats_plantation] is a [building] that produces oats. *)
val oats_plantation : building

(** [power_plant] is a [building] that produces electricity. *)
val power_plant : building

(** [mine] is a [building] that consumes electricity and produces iron. *)
val mine : building

(** [barrack] is a [building] that defends the city by increasing
    [defence] of buildlings. Details to be determined. *)
val barrack : building
*)

(** [building_name building] is the name of building [building]. *)
val building_name : building -> string

(** [new_resource amount name] is a resource with [name] and [amount]. *)
val new_resource : int -> string -> resource

(** [resource_name resource] is the name of resource [resource]. *)
val resource_name : resource -> string

(** [resource_amount resource] is the amount of resource [resource]. *)
val resource_amount : resource -> int

(** [resource_dependency building name] is the quantity of [name]
    dependency of [building]. *)
val resource_dependency : building -> string -> int

(** [new_building name cost maint out_a out_n tax defense res_a res_n] is a 
    new building with the parameters provided. *)
val new_building : string -> int -> int -> int -> string -> int -> int 
    -> int -> string -> building

(** [output building] is the name of [building]. *)
val building_name : building -> string

(** [output building] is the output of [building]. *)
val output : building -> resource

(** [tax_amount building] is the [tax] output of [building]. *)
val tax_amount : building -> int

(** [resource_sufficiency_check building resource] is [None] if the
    amount in [resource] is not sufficient for [building] to consume,
    and [Some resource] if [resource] is suffcient where [resource] is
    the updated [resource] after deducting the amount being consumed. *)
val resource_sufficiency_check : building -> resource -> resource option
