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

let house =
  {
    name = "house";
    cost = 0;
    maintenance = 0;
    output = { amount = 0; name = "" };
    tax = 0;
    defense = 0;
    resource_dependency = [];
  }

let oats_plantation =
  {
    name = "oats planation";
    cost = 0;
    maintenance = 0;
    output = { amount = 10; name = "oat" };
    tax = 0;
    defense = 0;
    resource_dependency = [];
  }

let power_plant =
  {
    name = "power_plant";
    cost = 0;
    maintenance = 0;
    output = { amount = 5; name = "electricity" };
    tax = 0;
    defense = 0;
    resource_dependency = [];
  }

let mine =
  {
    name = "mine";
    cost = 0;
    maintenance = 0;
    output = { amount = 1; name = "iron" };
    tax = 0;
    defense = 0;
    resource_dependency = [ { amount = 8; name = "electricity" } ];
  }

let barrack =
  {
    name = "barrack";
    cost = 0;
    maintenance = 0;
    output = { amount = 0; name = "" };
    tax = 0;
    defense = 0;
    resource_dependency = [];
  }

let new_resource name amount = { amount; name }

let resource_name (resource : resource) = resource.name

let resource_amount (resource : resource) = resource.amount

let get_resource_dependency building resource_name =
  let rec find_resource (lst : resource list) =
    match lst with
    | [] -> 0
    | h :: _ when h.name = resource_name -> h.amount
    | _ :: t -> find_resource t
  in
  find_resource building.resource_dependency

let get_output building resource_name =
  if building.output.name = resource_name then building.output.amount
  else 0

let get_tax bld = bld.tax

let input_resource_check building (resource : resource) =
  if get_resource_dependency building resource.name <= resource.amount
  then
    Some
      {
        name = resource.name;
        amount =
          resource.amount
          - get_resource_dependency building resource.name;
      }
  else None
