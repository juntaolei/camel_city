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
  resource_dependency : resource list;
}

type camel = { food : int }

let new_resource (name : string) (amount : int) : resource =
  (name, amount)

let resource_name (resource : resource) = fst resource

let resource_amount (resource : resource) = snd resource

let new_building
    name
    cost
    maintenance
    output
    income
    defense
    resource_dependency =
  {
    name;
    cost;
    maintenance;
    output;
    income;
    defense;
    resource_dependency;
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
