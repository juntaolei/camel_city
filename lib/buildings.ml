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

let house =
  {
    name = "house";
    cost = 0;
    maintenance = 2;
    output = ("money", 0);
    income = 3;
    defense = 0;
    resource_dependency = [];
  }

let oats_plantation =
  {
    name = "oats planation";
    cost = 0;
    maintenance = 0;
    output = ("oat", 1);
    income = 0;
    defense = 0;
    resource_dependency = [];
  }

let power_plant =
  {
    name = "power_plant";
    cost = 0;
    maintenance = 0;
    output = ("electricity", 5);
    income = 0;
    defense = 0;
    resource_dependency = [];
  }

let coal_mine =
  {
    name = "mine";
    cost = 0;
    maintenance = 0;
    output = ("coal", 5);
    income = 0;
    defense = 0;
    resource_dependency = [];
  }

let barrack =
  {
    name = "barrack";
    cost = 0;
    maintenance = 0;
    output = ("money", 0);
    income = 0;
    defense = 0;
    resource_dependency = [];
  }

let new_resource (name : string) (amount : int) : resource =
  (name, amount)

let resource_name (resource : resource) = fst resource

let resource_amount (resource : resource) = snd resource

(* let resource_dependency building name = let rec find_resource (lst :
   resource list) = match lst with | [] -> 0 | h :: _ when h.name = name
   -> h.amount | _ :: t -> find_resource t in find_resource
   building.resource_dependency *)

let output building = building.output

let resource_dependency building = building.resource_dependency

let income building = building.income

let maintenance building = building.maintenance
