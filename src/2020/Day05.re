open Belt;

let inputTest =
  Node_fs.readFileAsUtf8Sync("input/2020/day05test")
  ->Js.String2.trim(_)
  ->Js.String2.split("\n");

let input =
  Node_fs.readFileAsUtf8Sync("input/2020/day05")
  ->Js.String2.trim(_)
  ->Js.String2.split("\n");

let seatIds =
  input->Array.map(code => {
    let rowCode = code->Js.String2.substring(~from=0, ~to_=7);
    let colCode = code->Js.String2.substringToEnd(~from=7);
    let row =
      rowCode
      ->Js.String2.split("")
      ->Array.reduce(
          Array.range(0, 127),
          (acc, bf) => {
            let count = acc->Array.length;
            if (bf === "F") {
              acc->Array.slice(~offset=0, ~len=count / 2);
            } else {
              acc->Array.sliceToEnd(count / 2);
            };
          },
        )
      ->Array.getExn(0);
    let col =
      colCode
      ->Js.String2.split("")
      ->Array.reduce(
          Array.range(0, 7),
          (acc, lr) => {
            let count = acc->Array.length;
            if (lr === "L") {
              acc->Array.slice(~offset=0, ~len=count / 2);
            } else {
              acc->Array.sliceToEnd(count / 2);
            };
          },
        )
      ->Array.getExn(0);

    row * 8 + col;
  });

// part1
seatIds->Array.reduce(0, max)->Js.log;

let sorted = seatIds->List.fromArray->List.sort((a, b) => a - b);
let shifted =
  sorted
  ->List.drop(1)
  ->Option.flatMap(list => {
      Some(list->List.concat([seatIds->Array.reduce(0, max) + 1]))
    })
  ->Option.getExn;
let zipped = sorted->List.zip(shifted);

// part2
zipped
->List.keep(i => {
    let (a, b) = i;
    Js.Math.abs_int(a - b) > 1 ? true : false;
  })
->Js.log;
