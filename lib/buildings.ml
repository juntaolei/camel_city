type resource = {
  amount : int;
  name : string;
}

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
  tax : int;
  defense : int;
  resource_dependency : resource list;
}

type camel = { food : int }

let oat = { amount = 0; name = "oat" }

let electricity = { amount = 0; name = "electricity" }

let iron = { amount = 0; name = "iron" }

let money = { amount = 0; name = "money" }

let new_resource amount name = { amount = amount; name = name }

let resource_name (resource : resource) = resource.name

let resource_amount resource = resource.amount

let resource_dependency building name =
  let rec find_resource (lst : resource list) =
    match lst with
    | [] -> 0
    | h :: _ when h.name = name -> h.amount
    | _ :: t -> find_resource t
  in
  find_resource building.resource_dependency

let new_building n c m out_a out_n t d r_a r_n =
  {
    name = n;
    cost = c;
    maintenance = m;
    output = new_resource out_a out_n;
    tax = t;
    defense = d;
    resource_dependency = [new_resource r_a r_n] (* only single resource dependency for now*)
  }
let building_name building = building.name
  let output building = building.output

let tax_amount building = building.tax

let resource_sufficiency_check building (resource : resource) =
  if resource_dependency building resource.name <= resource.amount then
    Some
      {
        name = resource.name;
        amount =
          resource.amount - resource_dependency building resource.name;
      }
  else None
