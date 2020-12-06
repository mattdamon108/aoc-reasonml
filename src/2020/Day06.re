open Belt;

let inputTest =
  Node_fs.readFileAsUtf8Sync("input/2020/day06test")
  ->Js.String2.trim(_)
  ->Js.String2.split("\n\n");

let input =
  Node_fs.readFileAsUtf8Sync("input/2020/day06")
  ->Js.String2.trim(_)
  ->Js.String2.split("\n\n");

// part1
input
->Array.map(answer => {
    answer
    ->Js.String2.split("")
    ->Array.keep(char => char !== "\n")
    ->Set.String.fromArray
    ->Set.String.size
  })
->Array.reduce(0, (+))
->Js.log;

// part2
input
->Array.map(answer => {
    answer
    ->Js.String2.split("\n")
    ->Array.reduce("abcdefghijklmnopqrstuvwxyz", (dups, ans) => {
        ans
        ->Js.String2.split("")
        ->Array.keep(char => {dups->Js.String2.includes(char)})
        ->Js.String2.concatMany("", _)
      })
  })
->Array.map(dups => {dups->Js.String2.length})
->Array.reduce(0, (+))
->Js.log;
