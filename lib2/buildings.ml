type resource = string * int

type road = {
  cost : int;
  x : int;
  y : int;
}

type building = {
  name : string;
  cost : int;
  maintenance : int;
  output : resource;
  income : int;
  defense : int;
  happieness : int;
  resource_dependency : resource list;
  pop_dependency : int;
  housing : int;
  entertainment : int;
  final_building : bool;
}

type camel = { food : int }

let new_resource (name : string) (amount : int) : resource =
  (name, amount)

let resource_name (resource : resource) = fst resource

let resource_amount (resource : resource) = snd resource

(* let resource_dependency building name = let rec find_resource (lst :
   resource list) = match lst with | [] -> 0 | h :: _ when h.name = name
   -> h.amount | _ :: t -> find_resource t in find_resource
   building.resource_dependency *)

let new_building
    n
    c
    m
    out_a
    out_n
    t
    d
    r_a
    r_n
    happieness
    pop_dependency
    housing
    e
    (final_building:bool) =
  {
    name = n;
    cost = c;
    maintenance = m;
    output = new_resource out_n out_a;
    income = t;
    defense = d;
    happieness;
    resource_dependency =
      [ new_resource r_n r_a ]
      (* only single resource dependency for now*);
    pop_dependency = pop_dependency;
    housing = housing;
    entertainment = e;
    final_building = final_building;
  }

let new_road c x_coord y_coord = { cost = c; x = x_coord; y = y_coord }

let cost_rd (r : road) = r.cost

let building_name building = building.name

let output building = building.output

let cost building = building.cost

let resource_dependency building = building.resource_dependency

let income building = building.income

let maintenance building = building.maintenance

let defense building = building.defense

let happieness building = building.happieness

let get_pop_dependency building = building.pop_dependency

let housing building = building.housing

let entertainment building = building.entertainment
