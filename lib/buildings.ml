type building = {
  name : string;
  cost : int;
  maintenance : int;
  output : string * int;
  income : int;
  defense : int;
  resource_dependency : (string * int) list;
  happiness : int;
  population_dependency : int;
  housing : int;
  entertainment : int;
  is_final_building : bool;
}

type road = int * int

let new_resource name amount = (name, amount)

let resource_name resource = fst resource

let resource_amount resource = snd resource

let new_building
    name
    cost
    maintenance
    output
    income
    defense
    resource_dependency
    happiness
    population_dependency
    housing
    entertainment
    is_final_building =
  {
    name;
    cost;
    maintenance;
    output;
    income;
    defense;
    resource_dependency;
    happiness;
    population_dependency;
    housing;
    entertainment;
    is_final_building;
  }

let new_road x y = (x, y)

let decrease_defense building damage =
  { building with defense = building.defense - damage }
