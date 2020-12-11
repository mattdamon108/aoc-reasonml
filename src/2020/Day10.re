open Belt;

let inputTest =
  Node_fs.readFileAsUtf8Sync("input/2020/day10test")
  ->Js.String2.trim
  ->Js.String2.split("\n")
  ->Array.map(int_of_string);

let inputTest2 =
  Node_fs.readFileAsUtf8Sync("input/2020/day10test2")
  ->Js.String2.trim
  ->Js.String2.split("\n")
  ->Array.map(int_of_string);

let input =
  Node_fs.readFileAsUtf8Sync("input/2020/day10")
  ->Js.String2.trim
  ->Js.String2.split("\n")
  ->Array.map(int_of_string);

module Connection = {
  module FloatCmp =
    Belt.Id.MakeComparable({
      type t = float;
      let cmp = (a, b) => Pervasives.compare(a, b);
    });

  type t;

  let make = raw =>
    raw->List.fromArray->List.sort((a, b) => a - b)->List.toArray;
  let outlet = 0;
  let device = adapters => adapters->Array.reduce(0.0, max);

  let addOutletDevice = adapters =>
    [|0|]
    ->Array.concat(adapters)
    ->Array.concat([|adapters->Array.reduce(0, max) + 3|]);

  let createDependencies = data => {
    data->Array.reduce(Map.make(~id=(module FloatCmp)), (acc, adapter) => {
      acc->Map.set(
        adapter,
        data->Array.keep(a => {adapter < a && a <= adapter +. 3.0}),
      )
    });
  };

  let run = data => {
    let rec compute = (adapters, outlet, acc) => {
      switch (adapters->Array.get(0)) {
      | None => acc->Array.concat([|3|])
      | Some(j) =>
        let diff = j - outlet;
        let newAcc = acc->Array.concat([|diff|]);
        let restAdapters = adapters->Array.sliceToEnd(1);
        compute(restAdapters, j, newAcc);
      };
    };

    compute(data, 0, [||])->Utils.frequencies;
  };

  let occurance = (map, adapters, device) => {
    let occ = Map.make(~id=(module FloatCmp))->Map.set(0.0, 1.0);
    adapters
    ->Array.reduce(
        occ,
        (acc, adapter) => {
          let nextAdapters = map->Map.get(adapter);
          switch (nextAdapters) {
          | Some(na) =>
            na->Array.reduce(
              acc,
              (ac, nAdapter) => {
                let occurance = acc->Map.get(nAdapter);
                switch (occurance) {
                | Some(oc) =>
                  ac->Map.set(nAdapter, oc +. ac->Map.getExn(adapter))
                | None =>
                  ac->Map.set(nAdapter, 0.0 +. ac->Map.getExn(adapter))
                };
              },
            )
          | None => acc
          };
        },
      )
    ->Map.get(device);
  };

  let countOccurance = (dependencies, adapters, device) => {
    dependencies->occurance(adapters, device);
  };
};

let adapters =
  input
  ->Connection.make
  ->Connection.addOutletDevice
  ->Array.map(a => a->Float.fromInt);

let device = adapters->Connection.device;

// part1
input->Connection.make->Connection.run->Js.log;

// part2
adapters
->Connection.createDependencies
->Connection.countOccurance(adapters, device)
->Js.log;
