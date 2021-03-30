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

(** [new_resource name amount] is a resource with [name] and [amount]. *)
val new_resource : string -> int -> resource

(** [resource_name resource] is the name of resource [resource]. *)
val resource_name : resource -> string

(** [resource_amount resource] is the amount of resource [resource]. *)
val resource_amount : resource -> int

(** [resource_dependency name building] is the quantity of [name]
    dependency of [building]. *)
val resource_dependency : building -> string -> int

(** [output building] is the output of [building]. *)
val output : building -> resource

(** [tax_amount building] is the [tax] output of [building]. *)
val tax_amount : building -> int

(** [resource_sufficiency_check building resource] is [None] if the
    amount in [resource] is not sufficient for [building] to consume,
    and [Some resource] if [resource] is suffcient where [resource] is
    the updated [resource] after deducting the amount being consumed. *)
val resource_sufficiency_check : building -> resource -> resource option
