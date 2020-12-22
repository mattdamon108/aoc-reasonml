open Belt;

let inputTest =
  Node_fs.readFileAsUtf8Sync("input/2020/d21test")
  ->Js.String2.trim
  ->Js.String2.split("\n");

let input =
  Node_fs.readFileAsUtf8Sync("input/2020/d21")
  ->Js.String2.trim
  ->Js.String2.split("\n");

let parse = line => {
  let ingredients =
    line
    ->Js.String2.split(" (contains ")
    ->Array.getExn(0)
    ->Js.String2.split(" ");
  let allergens =
    line
    ->Js.String2.split(" (contains ")
    ->Array.getExn(1)
    ->Js.String2.split(", ")
    ->Array.map(a => a->Js.String2.replace(")", ""));
  (allergens, ingredients);
};

let rec duplicate = (xs, ys, dups) => {
  let length = xs->Array.length;
  if (length === 0) {
    dups;
  } else {
    let first = xs->Array.getExn(0);
    let rest = xs->Array.sliceToEnd(1);
    ys->Array.some(y => y === first)
      ? duplicate(rest, ys, dups->Array.concat([|first|]))
      : duplicate(rest, ys, dups);
  };
};

let mapOfAllergens =
  input
  ->Array.map(parse)
  ->Array.reduce(
      Map.String.empty,
      (acc, item) => {
        let (allergens, ingredients) = item;
        allergens->Array.reduce(
          acc,
          (ac, a) => {
            let prevIngredients = ac->Map.String.get(a);
            switch (prevIngredients) {
            | Some(pi) =>
              ac->Map.String.set(a, duplicate(pi, ingredients, [||]))
            | None => ac->Map.String.set(a, ingredients)
            };
          },
        );
      },
    );

let removeDups = map => {
  map->Map.String.reduce(Set.String.empty, (acc, k, v) => {
    v->Array.reduce(acc, (ac, v') => {ac->Set.String.add(v')})
  });
};

let allergens = mapOfAllergens->removeDups;

exception AllMultipleIngredients;

let rec removeDups2 = map => {
  let allUnique = map->Map.String.every((k, v) => {v->Array.length === 1});
  if (allUnique) {
    map;
  } else {
    let ingredients = map->Map.String.keep((k, v) => {v->Array.length === 1});
    let keys = ingredients->Map.String.keysToArray;
    let values =
      ingredients
      ->Map.String.valuesToArray
      ->Array.reduce([||], (acc, i) => {acc->Array.concat(i)});
    let newMap =
      map->Map.String.mapWithKey((k, v) => {
        keys->Array.some(kys => kys === k)
          ? v
          : {
            v->Array.keep(v' => {!values->Array.some(vls => vls === v')});
          }
      });

    removeDups2(newMap);
  };
};

// part1
input
->Array.map(parse)
->Array.reduce(
    [||],
    (acc, i) => {
      let (_, ingredients) = i;
      acc->Array.concat(ingredients);
    },
  )
->Array.keep(i => {!allergens->Set.String.some(a => a === i)})
->Array.length
->Js.log;

// part2
mapOfAllergens
->removeDups2
->Map.String.toList
->List.sort((a, b) => Pervasives.compare(a, b))
->List.toArray
->Array.map(i => {
    let (_, ingredients) = i;
    ingredients->Array.getExn(0);
  })
->Array.reduce("", (acc, i) => {
    acc->Js.String2.concat(i)->Js.String2.concat(",")
  })
->Js.log;
