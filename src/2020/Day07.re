open Belt;

let inputTest =
  Node_fs.readFileAsUtf8Sync("input/2020/day07test")
  ->Js.String2.trim(_)
  ->Js.String2.split("\n");

let inputTest2 =
  Node_fs.readFileAsUtf8Sync("input/2020/day07test2")
  ->Js.String2.trim(_)
  ->Js.String2.split("\n");

let input =
  Node_fs.readFileAsUtf8Sync("input/2020/day07")
  ->Js.String2.trim(_)
  ->Js.String2.split("\n");

let parseBag = bag => {
  let regexChildrenBag = [%re "/^(\d)\s([A-z]+\s[A-z]+)\sbags?\.?/g"];
  let%Opt result = Js.Re.exec_(regexChildrenBag, bag);
  let%Opt countStr =
    result->Js.Re.captures->Array.getExn(1)->Js.Nullable.toOption;
  let%Opt bag = result->Js.Re.captures->Array.getExn(2)->Js.Nullable.toOption;
  let count = countStr->int_of_string;
  Some((count, bag));
};

let mapOfBagReverse =
  input
  ->Array.map(line => {
      let splitted = line->Js.String2.split(" contain ");
      let parentBag =
        splitted
        ->Array.getExn(0)
        ->Js.String2.replace(" bags", "")
        ->Js.String2.replace(" bag", "");
      let matched =
        splitted
        ->Array.getExn(1)
        ->Js.String2.replace(".", "")
        ->Js.String2.split(", ")
        ->Array.map(bag => bag->parseBag);
      (parentBag, matched);
    })
  ->Array.reduce(
      Map.String.empty,
      (map, line) => {
        let (parentBag, childrenBag) = line;
        childrenBag->Array.reduce(map, (acc, bag) => {
          switch (bag) {
          | Some((_, b)) =>
            switch (acc->Map.String.get(b)) {
            | Some(p) =>
              acc->Map.String.set(b, p->Array.concat([|parentBag|]))
            | None => acc->Map.String.set(b, [|parentBag|])
            }
          | _ => acc
          }
        });
      },
    );

let mapOfBag =
  input
  ->Array.map(line => {
      let splitted = line->Js.String2.split(" contain ");
      let parentBag =
        splitted
        ->Array.getExn(0)
        ->Js.String2.replace(" bags", "")
        ->Js.String2.replace(" bag", "");
      let matched =
        splitted
        ->Array.getExn(1)
        ->Js.String2.replace(".", "")
        ->Js.String2.split(", ")
        ->Array.map(bag => bag->parseBag);
      (parentBag, matched);
    })
  ->Array.reduce(
      Map.String.empty,
      (map, line) => {
        let (parentBag, childrenBag) = line;
        map->Map.String.set(parentBag, childrenBag);
      },
    );

let rec counter = (map, bag, acc) => {
  let containers =
    bag->Array.map(b => map->Map.String.getWithDefault(b, [||]));
  let spread =
    containers->Array.reduce([||], (acc, bags) => {acc->Array.concat(bags)});
  if (spread->Array.length === 0) {
    acc;
  } else {
    counter(map, spread, acc->Array.concat(spread));
  };
};

let rec counter2 = (map, bag, count) => {
  let children: array((int, string)) =
    (bag: array((int, string)))
    ->Array.reduce(
        [||],
        (acc, b) => {
          let (ct, name) = b;
          map
          ->Map.String.getWithDefault(name, [||])
          ->Array.reduce(acc, (ac, child) => {
              switch (child) {
              | Some((c, n)) => ac->Array.concat([|(ct * c, n)|])
              | None => acc
              }
            });
        },
      );

  let newCount =
    children->Array.reduce(
      count,
      (acc, bag) => {
        let (ct, _) = bag;
        acc + ct;
      },
    );

  if (children->Array.length === 0) {
    newCount;
  } else {
    counter2(map, children, newCount);
  };
};

// part1
counter(mapOfBagReverse, [|"shiny gold"|], [||])
->Array.reduce(Set.String.empty, (acc, bag) => {acc->Set.String.add(bag)})
->Set.String.size
->Js.log;

// part2
counter2(mapOfBag, [|(1, "shiny gold")|], 0)->Js.log;
