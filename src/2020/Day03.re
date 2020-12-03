open Belt;

let inputTest =
  Node_fs.readFileAsUtf8Sync("input/2020/day03test")
  ->Js.String2.trim(_)
  ->Js.String2.split("\n");

let input =
  Node_fs.readFileAsUtf8Sync("input/2020/day03")
  ->Js.String2.trim(_)
  ->Js.String2.split("\n");

module CoordCmp =
  Id.MakeComparable({
    type t = (int, int);
    let cmp = (a, b) => Pervasives.compare(a, b);
  });

let width = input->Array.getExn(0)->Js.String2.length;
let height = input->Array.length;

let mapOfTree =
  input
  ->Array.mapWithIndex((idxY, slope) => {
      slope
      ->Js.String2.split("")
      ->Array.mapWithIndex((idxX, point) =>
          if (point === "#") {
            ((idxX, idxY), 1);
          } else {
            ((idxX, idxY), 0);
          }
        )
    })
  ->Array.reduce(Map.make(~id=(module CoordCmp)), (acc, i) => {
      i->Array.reduce(
        acc,
        (a, point) => {
          let ((x, y), v) = point;
          Map.set(a, (x, y), v);
        },
      )
    });

let toboggan = (map, wide, long, startPoint, moveX, moveY) => {
  let (x, y) = startPoint;
  let rec traverse = (x, y, count) =>
    if (y < long) {
      let modX = x mod wide;
      let isTree = map->Map.getExn((modX, y)) === 1;
      let newCount = isTree ? count + 1 : count;
      traverse(x + moveX, y + moveY, newCount);
    } else {
      count;
    };

  traverse(x, y, 0);
};

// part1
toboggan(mapOfTree, width, height, (0, 0), 3, 1)->Js.log;

// part2
let r1d1 = toboggan(mapOfTree, width, height, (0,0), 1,1)->Float.fromInt;
let r3d1 = toboggan(mapOfTree, width, height, (0,0), 3,1)->Float.fromInt;
let r5d1 = toboggan(mapOfTree, width, height, (0,0), 5,1)->Float.fromInt;
let r7d1 = toboggan(mapOfTree, width, height, (0,0), 7,1)->Float.fromInt;
let r1d2 = toboggan(mapOfTree, width, height, (0,0), 1,2)->Float.fromInt;
(r1d1 *. r3d1 *. r5d1 *. r7d1 *. r1d2)->Js.log;