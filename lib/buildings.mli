(** [resource] is the type of resources consumed and produced by camel city. *)
type resource

(** [road] is a type of basic construction unit in the map. *)
type road

(** [building] is a type of basic construction unit in the map that is able
    to produce resources, tax, and some requires resources as input. 
    Need money to purchase. *)
type building

(** [camel] is the type of camel residents that consume oats and supports 
    operations of buildings. *)
type camel

(** [oat] is a [resource] consumed by camels and produced by oat_plantation. *)
val oat : resource

(** [oat] is a [resource] consumed by camels. *)
val electricity : resource

(** [oat] is a [resource] consumed by mines. *)
val iron : resource

(** [money] is a [resource] collected from buildings. *)
val money : resource

(** [house] is a [building] that allows camel residents, thereby increasing 
    camelcity's population. *)
val house : building

(** [oats_plantation] is a [building] that produces oats. *)
val oats_plantation : building

(** [power_plant] is a [building] that produces electricity. *)
val power_plant : building

(** [mine] is a [building] that consumes electricity and produces iron. *)
val mine : building

(** [barrack] is a [building] that defends the city by increasing [defence]
    of buildlings. Details to be determined *)
val barrack : building

(** [new_resource num str] is the [resource] with name [str] and amount
    [num]. *)
val new_resource : string -> int -> resource

(** [resource_name res] is the name of resource [res] *)
val resource_name : resource -> string

(** [resource_amount res] is the amount of resource [res] *)
val resource_amount : resource -> int

(** [get_resource_dependency resource_name bld] is the quantity of 
    [resource_name] dependency of [bld]*)
val get_resource_dependency : building -> string -> int

(** [get_output resource_name bld] is the [resource_name] output of [bld]*)
val get_output : building -> string -> int

(** [get_tax bld] is the [tax] output of [bld]*)
val get_tax : building -> int

(** [input_resource_check bld res] is [None] if the amount in [res] is not
    sufficient for [bld] to consume, and [Some resource] if [res] is suffcient
    where [resource] is the updated [res] after deducting the amount
    being consumed. *)
val input_resource_check : building -> resource -> resource option
