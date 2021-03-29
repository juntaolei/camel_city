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

let new_resource str num = { amount = num; name = str }

let resource_name (res : resource) = res.name

let resource_amount (res : resource) = res.amount

let get_resource_dependency bld resource_name =
  let rec find_resource (lst : resource list) =
    match lst with
    | [] -> 0
    | h :: t ->
        if h.name = resource_name then h.amount else find_resource t
  in
  find_resource bld.resource_dependency

let get_output bld resource_name =
  if bld.output.name = resource_name then bld.output.amount else 0

let get_tax bld = bld.tax

let input_resource_check (bld : building) (res : resource) =
  if get_resource_dependency bld res.name <= res.amount then
    Some
      {
        name = res.name;
        amount = res.amount - get_resource_dependency bld res.name;
      }
  else None
