open Belt;

let inputTest =
  Node_fs.readFileAsUtf8Sync("input/2020/d17test")
  ->Js.String2.trim
  ->Js.String2.split("\n");

let input =
  Node_fs.readFileAsUtf8Sync("input/2020/d17")
  ->Js.String2.trim
  ->Js.String2.split("\n");

type status =
  | Active
  | Inactive;

module ThreeDimCmp =
  Id.MakeComparable({
    type t = (int, int, int);
    let cmp = (a, b) => Pervasives.compare(a, b);
  });

let parse = data => {
  data->Array.reduceWithIndex(
    Map.make(~id=(module ThreeDimCmp)), (acc, row, idxY) => {
    row
    ->Js.String2.split("")
    ->Array.reduceWithIndex(acc, (ac, col, idxX) => {
        ac->Map.set((idxX - 1, idxY - 1, 0), col === "#" ? Active : Inactive)
      })
  });
};

let adjacents = ((x, y, z)) => {
  let xs = Array.range(x - 1, x + 1);
  let ys = Array.range(y - 1, y + 1);
  let zs = Array.range(z - 1, z + 1);

  xs
  ->Array.reduce([||], (acc, xi) => {
      ys->Array.reduce(acc, (ac, yi) => {
        zs->Array.reduce(ac, (a, zi) => {a->Array.concat([|(xi, yi, zi)|])})
      })
    })
  ->Array.keep(i => {Pervasives.compare(i, (x, y, z)) !== 0});
};

let checkCube = (map, pos, status) => {
  let (countOfActive, countOfInactive) =
    pos
    ->adjacents
    ->Array.reduce(
        (0, 0),
        (acc, cube) => {
          let (ca, ci) = acc;
          switch (map->Map.get(cube)) {
          | Some(s) => s === Active ? (ca + 1, ci) : (ca, ci + 1)
          | None => acc
          };
        },
      );
  switch (status) {
  | Active => countOfActive === 2 || countOfActive === 3 ? Active : Inactive
  | Inactive => countOfActive === 3 ? Active : Inactive
  };
};

let makeSurround = map => {
  let (maxX, maxY, maxZ) =
    map->Map.reduce(
      (0, 0, 0),
      (acc, k, v) => {
        let (ax, ay, az) = acc;
        let (x, y, z) = k;
        (max(ax, x), max(ay, y), max(az, z));
      },
    );
  let (minX, minY, minZ) =
    map->Map.reduce(
      (max_int, max_int, max_int),
      (acc, k, v) => {
        let (ax, ay, az) = acc;
        let (x, y, z) = k;
        (min(ax, x), min(ay, y), min(az, z));
      },
    );

  let xs = Array.range(minX - 1, maxX + 1);
  let ys = Array.range(minY - 1, maxY + 1);
  let zs = Array.range(minZ - 1, maxZ + 1);

  xs->Array.reduce(Map.make(~id=(module ThreeDimCmp)), (acc, xi) => {
    ys->Array.reduce(acc, (ac, yi) => {
      zs->Array.reduce(ac, (a, zi) => {a->Map.set((xi, yi, zi), Inactive)})
    })
  });
};

let cycle = (cubes, count) => {
  let rec run = (map, n) =>
    if (n === count) {
      map;
    } else {
      let surround = makeSurround(map);
      let newMap =
        surround->Map.mapWithKey((k, v) => {
          switch (map->Map.get(k)) {
          | Some(v') => checkCube(map, k, v')
          | None => checkCube(map, k, v)
          }
        });
      run(newMap, n + 1);
    };

  run(cubes, 0);
};

let countActive = cubes => {
  cubes->Map.reduce(0, (acc, k, v) => {v === Active ? acc + 1 : acc});
};

// part1
input->parse->cycle(6)->countActive->Js.log;

module FourDimCmp =
  Id.MakeComparable({
    type t = (int, int, int, int);
    let cmp = (a, b) => Pervasives.compare(a, b);
  });

let parse2 = data => {
  data->Array.reduceWithIndex(
    Map.make(~id=(module FourDimCmp)), (acc, row, idxY) => {
    row
    ->Js.String2.split("")
    ->Array.reduceWithIndex(acc, (ac, col, idxX) => {
        ac->Map.set(
          (idxX - 1, idxY - 1, 0, 0),
          col === "#" ? Active : Inactive,
        )
      })
  });
};

let adjacents2 = ((x, y, z, w)) => {
  let xs = Array.range(x - 1, x + 1);
  let ys = Array.range(y - 1, y + 1);
  let zs = Array.range(z - 1, z + 1);
  let ws = Array.range(w - 1, w + 1);

  xs
  ->Array.reduce([||], (accc, xi) => {
      ys->Array.reduce(accc, (acc, yi) => {
        zs->Array.reduce(acc, (ac, zi) => {
          ws->Array.reduce(ac, (a, wi) => {
            a->Array.concat([|(xi, yi, zi, wi)|])
          })
        })
      })
    })
  ->Array.keep(i => {Pervasives.compare(i, (x, y, z, w)) !== 0});
};

let checkCube2 = (map, pos, status) => {
  let (countOfActive, countOfInactive) =
    pos
    ->adjacents2
    ->Array.reduce(
        (0, 0),
        (acc, cube) => {
          let (ca, ci) = acc;
          switch (map->Map.get(cube)) {
          | Some(s) => s === Active ? (ca + 1, ci) : (ca, ci + 1)
          | None => acc
          };
        },
      );
  switch (status) {
  | Active => countOfActive === 2 || countOfActive === 3 ? Active : Inactive
  | Inactive => countOfActive === 3 ? Active : Inactive
  };
};

let makeSurround2 = map => {
  let (maxX, maxY, maxZ, maxW) =
    map->Map.reduce(
      (0, 0, 0, 0),
      (acc, k, v) => {
        let (ax, ay, az, aw) = acc;
        let (x, y, z, w) = k;
        (max(ax, x), max(ay, y), max(az, z), max(aw, w));
      },
    );
  let (minX, minY, minZ, minW) =
    map->Map.reduce(
      (max_int, max_int, max_int, max_int),
      (acc, k, v) => {
        let (ax, ay, az, aw) = acc;
        let (x, y, z, w) = k;
        (min(ax, x), min(ay, y), min(az, z), min(aw, w));
      },
    );

  let xs = Array.range(minX - 1, maxX + 1);
  let ys = Array.range(minY - 1, maxY + 1);
  let zs = Array.range(minZ - 1, maxZ + 1);
  let ws = Array.range(minW - 1, maxW + 1);

  xs->Array.reduce(Map.make(~id=(module FourDimCmp)), (accc, xi) => {
    ys->Array.reduce(accc, (acc, yi) => {
      zs->Array.reduce(acc, (ac, zi) => {
        ws->Array.reduce(ac, (a, wi) => {
          a->Map.set((xi, yi, zi, wi), Inactive)
        })
      })
    })
  });
};

let cycle2 = (cubes, count) => {
  let rec run = (map, n) =>
    if (n === count) {
      map;
    } else {
      let surround = makeSurround2(map);
      let newMap =
        surround->Map.mapWithKey((k, v) => {
          switch (map->Map.get(k)) {
          | Some(v') => checkCube2(map, k, v')
          | None => checkCube2(map, k, v)
          }
        });
      run(newMap, n + 1);
    };

  run(cubes, 0);
};

// part2
input->parse2->cycle2(6)->countActive->Js.log;
