open Belt;

let inputTest =
  Node_fs.readFileAsUtf8Sync("input/2020/day11test")
  ->Js.String2.trim
  ->Js.String2.split("\n");

let input =
  Node_fs.readFileAsUtf8Sync("input/2020/day11")
  ->Js.String2.trim
  ->Js.String2.split("\n");

module CoordCmp =
  Id.MakeComparable({
    type t = (int, int);
    let cmp = (a, b) => Pervasives.compare(a, b);
  });

type seat =
  | Floor // 0 "."
  | Occupied // 1 "#"
  | Empty; // 2 "L"

let parse = raw => {
  raw->Array.reduceWithIndex(
    Map.make(~id=(module CoordCmp)), (acc, seats, idxY) => {
    seats
    ->Js.String2.split("")
    ->Array.reduceWithIndex(acc, (ac, seat, idxX) => {
        ac->Map.set(
          (idxX, idxY),
          seat === "." ? Floor : seat === "#" ? Occupied : Empty,
        )
      })
  });
};

let adjacents = coord => {
  let (x, y) = coord;
  [|
    (x, y - 1),
    (x + 1, y),
    (x, y + 1),
    (x - 1, y),
    (x - 1, y - 1),
    (x + 1, y - 1),
    (x - 1, y + 1),
    (x + 1, y + 1),
  |];
};

let range = map => {
  map->Map.reduce(
    (0, 0),
    (acc, k, v) => {
      let (x, y) = acc;
      let (kx, ky) = k;
      (max(x, kx), max(y, ky));
    },
  );
};

let checkCoord = (map, coord, status) => {
  let (occ, ety) =
    adjacents(coord)
    ->Array.reduce(
        (0, 0),
        (acc, adj) => {
          let (occupied, empty) = acc;
          let state = map->Map.get(adj);
          switch (state) {
          | Some(s) =>
            switch (s) {
            | Occupied => (occupied + 1, empty)
            | Empty => (occupied, empty + 1)
            | Floor => acc
            }
          | None => acc
          };
        },
      );
  if (status === Empty && occ === 0) {
    Occupied;
  } else if (status === Occupied && occ >= 4) {
    Empty;
  } else {
    status;
  };
};

let checkCoord2 = (map, coord, status) =>
  if (status === Floor) {
    status;
  } else {
    let (x, y) = coord;
    let (mx, my) = map->range;

    let seek = coords => {
      coords
      ->Array.reduce([||], (acc, crd) => {
          switch (map->Map.get(crd)) {
          | Some(v) => acc->Array.concat([|v|])
          | None => acc
          }
        })
      ->Array.keep(state => state !== Floor)
      ->Array.get(0);
    };

    let up =
      Array.range(0, y - 1)->Array.reverse->Array.map(i => (x, i))->seek;
    let right = Array.range(x + 1, mx)->Array.map(i => (i, y))->seek;
    let down = Array.range(y + 1, my)->Array.map(i => (x, i))->seek;
    let left =
      Array.range(0, x - 1)->Array.reverse->Array.map(i => (i, y))->seek;
    let upleft =
      Array.range(0, x - 1)
      ->Array.reverse
      ->Array.zip(Array.range(0, y - 1)->Array.reverse)
      ->seek;
    let upright =
      Array.range(x + 1, mx)
      ->Array.zip(Array.range(0, y - 1)->Array.reverse)
      ->seek;
    let downleft =
      Array.range(0, x - 1)
      ->Array.reverse
      ->Array.zip(Array.range(y + 1, my))
      ->seek;
    let downright =
      Array.range(x + 1, mx)->Array.zip(Array.range(y + 1, my))->seek;

    let (occ, ety) =
      [|up, right, down, left, upleft, upright, downleft, downright|]
      ->Array.reduce(
          (0, 0),
          (acc, state) => {
            let (o, e) = acc;
            switch (state) {
            | Some(Occupied) => (o + 1, e)
            | Some(Empty) => (o, e + 1)
            | _ => acc
            };
          },
        );

    if (status === Empty && occ === 0) {
      Occupied;
    } else if (status === Occupied && occ >= 5) {
      Empty;
    } else {
      status;
    };
  };

let rec scanner = (f, map) => {
  let (newMap, changes) =
    map->Map.reduce(
      (Map.make(~id=(module CoordCmp)), 0),
      (acc, k, v) => {
        let (accMap, count) = acc;
        let result = f(map, k, v);
        let newCount = result !== v ? count + 1 : count;
        (accMap->Map.set(k, result), newCount);
      },
    );
  changes->Js.log;
  if (changes === 0) {
    newMap->Map.reduce(0, (acc, k, v) => {
      switch (v) {
      | Occupied => acc + 1
      | _ => acc
      }
    });
  } else {
    scanner(f, newMap);
  };
};

// part1
// input->parse->scanner(checkCoord, _)->Js.log;

input->parse->scanner(checkCoord2, _)->Js.log;
