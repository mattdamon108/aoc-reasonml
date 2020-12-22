open Belt;

let inputTest =
  Node_fs.readFileAsUtf8Sync("input/2020/d20test")
  ->Js.String2.trim
  ->Js.String2.split("\n\n");

let input =
  Node_fs.readFileAsUtf8Sync("input/2020/d20")
  ->Js.String2.trim
  ->Js.String2.split("\n\n");

module CoordCmp =
  Id.MakeComparable({
    type t = (int, int);
    let cmp = (a, b) => Pervasives.compare(a, b);
  });

let parse = image => {
  let lines = image->Js.String2.split("\n");
  let reTile = [%re "/\d+/"];
  let result = lines->Array.getExn(0) |> Js.Re.exec_(reTile);
  let title =
    switch (result) {
    | Some(r) =>
      r->Js.Re.captures->Array.getExn(0)->Js.Nullable.toOption->Option.getExn
    | None => ""
    };

  let pixels =
    lines
    ->Array.sliceToEnd(1)
    ->Array.reduceWithIndex(
        Map.make(~id=(module CoordCmp)), (acc, line, idxY) => {
        line
        ->Js.String2.split("")
        ->Array.reduceWithIndex(acc, (ac, char, idxX) => {
            ac->Map.set((idxX, idxY), char)
          })
      });

  (title, pixels);
};

let parsed =
  inputTest
  ->Array.map(parse)
  ->Array.map(tile => {
      let (title, pixels) = tile;
      let top =
        pixels->Map.reduce(
          [||],
          (acc, k, v) => {
            let (x, y) = k;
            y === 0 ? acc->Array.concat([|v|]) : acc;
          },
        );
      let right =
        pixels->Map.reduce(
          [||],
          (acc, k, v) => {
            let (x, y) = k;
            x === 9 ? acc->Array.concat([|v|]) : acc;
          },
        );
      let bottom =
        pixels->Map.reduce(
          [||],
          (acc, k, v) => {
            let (x, y) = k;
            y === 9 ? acc->Array.concat([|v|]) : acc;
          },
        );
      let left =
        pixels->Map.reduce(
          [||],
          (acc, k, v) => {
            let (x, y) = k;
            x === 0 ? acc->Array.concat([|v|]) : acc;
          },
        );
      (
        title,
        [|
          top,
          top->Array.reverse,
          right,
          right->Array.reverse,
          bottom,
          bottom->Array.reverse,
          left,
          left->Array.reverse,
        |],
      );
    });

let findCorner = (xs, ys) => {
  xs->Array.reduce(Map.String.empty, (acc, x) => {
    ys->Array.reduce(
      acc,
      (ac, y) => {
        let (titleX, edgesX) = x;
        let (titleY, edgesY) = y;

        let matchedEdges =
          edgesX
          ->Array.reduce([||], (xcc, ex) => {
              edgesY->Array.reduce(xcc, (ycc, ey) => {
                Pervasives.compare(ex, ey) === 0 && titleX !== titleY
                  ? ycc->Array.concat([|ex, ey|]) : ycc
              })
            })
          ->Array.reduce([||], (acc, item) => {
              acc->Array.some(i => Pervasives.compare(i, item) === 0)
                ? acc : acc->Array.concat([|item|])
            });

        let prevMatches = ac->Map.String.get(titleX);
        switch (prevMatches) {
        | Some(pm) =>
          ac->Map.String.set(titleX, pm + matchedEdges->Array.length / 2)
        | None => ac->Map.String.set(titleX, matchedEdges->Array.length / 2)
        };
      },
    )
  });
};

let findSides = (xs, ys) => {
  xs->Array.reduce(Map.String.empty, (acc, x) => {
    ys->Array.reduce(
      acc,
      (ac, y) => {
        let (titleX, edgesX) = x;
        let (titleY, edgesY) = y;

        let matchedEdges =
          edgesX
          ->Array.reduce([||], (xcc, ex) => {
              edgesY->Array.reduce(xcc, (ycc, ey) => {
                Pervasives.compare(ex, ey) === 0 && titleX !== titleY
                  ? ycc->Array.concat([|ex, ey|]) : ycc
              })
            })
          ->Array.reduce([||], (acc, item) => {
              acc->Array.some(i => Pervasives.compare(i, item) === 0)
                ? acc : acc->Array.concat([|item|])
            });

        matchedEdges->Array.length >= 2
          ? {
            let prevMatches = ac->Map.String.get(titleX);
            switch (prevMatches) {
            | Some(pm) =>
              ac->Map.String.set(titleX, pm->Array.concat([|titleY|]))
            | None => ac->Map.String.set(titleX, [|titleY|])
            };
          }
          : ac;
      },
    )
  });
};

let generateCanvas = (images, total) => {
  let length = sqrt(total->float_of_int)->int_of_float;
  Array.range(0, length -1)->Array.reduceWithIndex(Map.make(~id=(module CoordCmp)), (acc, i, idxY) => {
    Array.range(0, length - 1)->Array.reduceWithIndex(acc, (ac,j, idxX) => {
      ac->Map.set((idxX, idxY), "")
    })
  })
}

// part1
// findCorner(parsed, parsed)
// ->Map.String.keep((k, v) => {v <= 2})
// ->Map.String.keysToArray->Array.reduce(1.0, (acc, item) => {
//   acc *. item->float_of_string
// })
// ->Js.log;

findSides(parsed, parsed)->Map.String.valuesToArray->Js.log;

exception UndefinedCoord;
exception UndefinedTitle;

let arrangeImages = (images) => {
  let mapOfSides = findSides(images, images);
  let total = images->Array.length;
  let lengthOfSide = sqrt(total->float_of_int)->int_of_float;
  let corners = findCorner(images,images)->Map.String.keep((k,v) => v<=2)->Map.String.keysToArray
  let canvas = generateCanvas(images, total)->Map.set((0,0), corners->Array.getExn(0))
  ->Map.set((0,lengthOfSide-1), corners->Array.getExn(1))
  ->Map.set((lengthOfSide-1,0), corners->Array.getExn(2))
  ->Map.set((lengthOfSide-1,lengthOfSide-1), corners->Array.getExn(3));
  
  
}
