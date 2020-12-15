open Belt;

let inputTest =
  Node_fs.readFileAsUtf8Sync("input/2020/day14test")
  ->Js.String2.trim
  ->Js.String2.split("\n");

let inputTest2 =
  Node_fs.readFileAsUtf8Sync("input/2020/day14test2")
  ->Js.String2.trim
  ->Js.String2.split("\n");

let input =
  Node_fs.readFileAsUtf8Sync("input/2020/day14")
  ->Js.String2.trim
  ->Js.String2.split("\n");

let parseMem = mem => {
  let reMem = [%re "/^mem\[(\d+)\]\s=\s(\d+)$/g"];
  let%Opt result = Js.Re.exec_(reMem, mem);
  let%Opt pos = result->Js.Re.captures->Array.getExn(1)->Js.Nullable.toOption;
  let%Opt value =
    result->Js.Re.captures->Array.getExn(2)->Js.Nullable.toOption;
  Some((pos->float_of_string, value->float_of_string));
};

let parseMask = (mask, replacement) => {
  mask
  ->Js.String2.split("")
  ->Array.map(i => {i === "X" ? replacement : i})
  ->Array.concat([|"0b"|], _)
  ->Js.String2.concatMany("", _);
};

let parseMask2 = mask => {
  mask
  ->Js.String2.split("")
  ->Array.reverse
  ->Array.mapWithIndex((idx, m) => {(idx, m)})
  ->Array.keep(msk => {
      let (_, m) = msk;
      m === "X";
    });
};

let masking = (value, mask) => {
  /**
  * Js bitwise 연산은 32bit 이상에서 지원이 안되는 듯..
  */
  let bitmaskAND = mask->parseMask("1")->Int64.of_string;
  let bitmaskOR = mask->parseMask("0")->Int64.of_string;
  value
  ->Int64.of_float
  ->Int64.logand(bitmaskAND)
  ->Int64.logor(bitmaskOR)
  ->Int64.to_float;
};

let masking2 = (address, mask) => {
  let bitmaskOR = mask->parseMask("0")->Int64.of_string;
  let maskedOr = address->Int64.of_float->Int64.logor(bitmaskOR);

  parseMask2(mask)
  ->Array.reduce(
      [|maskedOr|],
      (acc, m) => {
        let (offset, _) = m;
        acc->Array.reduce(
          [||],
          (ac, i) => {
            let overwrittenZero =
              i->Int64.logand(
                1->Int64.of_int->Int64.shift_left(offset)->Int64.lognot,
              );
            let overwrittenOne =
              i->Int64.logor(1->Int64.of_int->Int64.shift_left(offset));
            ac->Array.concat([|overwrittenZero, overwrittenOne|]);
          },
        );
      },
    );
};

module FloatCmp =
  Id.MakeComparable({
    type t = float;
    let cmp = (a, b) => Pervasives.compare(a, b);
  });

let (map, mask) =
  input->Array.reduce(
    (Map.make(~id=(module FloatCmp)), ""),
    (acc, code) => {
      let (accMap, accMask) = acc;
      let splitted = code->Js.String2.split(" = ");
      let command = splitted->Array.getExn(0);
      if (command === "mask") {
        (accMap, splitted->Array.getExn(1));
      } else {
        switch (parseMem(code)) {
        | Some((p, v)) =>
          let value = masking(v, accMask);
          (accMap->Map.set(p, value), accMask);
        | None => acc
        };
      };
    },
  );

let (map2, mask) =
  input->Array.reduce(
    (Map.make(~id=(module FloatCmp)), ""),
    (acc, code) => {
      let (accMap, accMask) = acc;
      let splitted = code->Js.String2.split(" = ");
      let command = splitted->Array.getExn(0);
      if (command === "mask") {
        (accMap, splitted->Array.getExn(1));
      } else {
        switch (parseMem(code)) {
        | Some((a, v)) =>
          let addresses = masking2(a, accMask);
          let newMap =
            addresses->Array.reduce(accMap, (acc, adr) => {
              acc->Map.set(adr->Int64.to_float, v)
            });
          (newMap, accMask);
        | None => acc
        };
      };
    },
  );

// part1
map->Map.reduce(0.0, (acc, _, v) => {acc +. v})->Js.log;

// part2
map2->Map.reduce(0.0, (acc, _, v) => {acc +. v})->Js.log;
