let inputTest =
  Node_fs.readFileAsUtf8Sync("input/2020/day02test")
  ->Js.String2.trim(_)
  ->Js.String2.split("\n")
  ->Belt.Array.map(i => i->Js.String2.split(" "));

let input =
  Node_fs.readFileAsUtf8Sync("input/2020/day02")
  ->Js.String2.trim(_)
  ->Js.String2.split("\n")
  ->Belt.Array.map(i => i->Js.String2.split(" "));

let checkValid = (range, char, password) => {
  let rg = range->Js.String2.split("-");
  let least =
    rg->Belt.Array.getExn(0)->Belt.Int.fromString->Belt.Option.getExn;
  let most =
    rg->Belt.Array.getExn(1)->Belt.Int.fromString->Belt.Option.getExn;
  let c = char->Js.String2.substring(~from=0, ~to_=1);
  let counted =
    password
    ->Js.String2.split("")
    ->Belt.Array.keep(i => i == c)
    ->Belt.Array.length;
  least <= counted && counted <= most;
};

let checkValid2 = (range, char, password) => {
  let rg = range->Js.String2.split("-");
  let firstPos =
    rg->Belt.Array.getExn(0)->Belt.Int.fromString->Belt.Option.getExn;
  let secondPos =
    rg->Belt.Array.getExn(1)->Belt.Int.fromString->Belt.Option.getExn;
  let c = char->Js.String2.substring(~from=0, ~to_=1);
  let splittedPW = password->Js.String2.split("");
  let match1 = splittedPW->Belt.Array.get(firstPos - 1);
  let match2 = splittedPW->Belt.Array.get(secondPos - 1);
  let result1 =
    switch (match1) {
    | Some(m1) => m1 == c
    | None => false
    };
  let result2 =
    switch (match2) {
    | Some(m2) => m2 == c
    | None => false
    };
  switch (result1, result2) {
  | (true, true) => false
  | (false, false) => false
  | (_, _) => true
  };
};

// part1
input
->Belt.Array.keep(i => {
    let range = i->Belt.Array.getExn(0);
    let char = i->Belt.Array.getExn(1);
    let password = i->Belt.Array.getExn(2);
    checkValid(range, char, password);
  })
->Belt.Array.length
->Js.log;

// part2
input
->Belt.Array.keep(i => {
    let range = i->Belt.Array.getExn(0);
    let char = i->Belt.Array.getExn(1);
    let password = i->Belt.Array.getExn(2);
    checkValid2(range, char, password);
  })
->Belt.Array.length
->Js.log;
