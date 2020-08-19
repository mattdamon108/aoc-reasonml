/*
 * Advent of Code 2017 Day06
 */

open Belt;

let input =
  Node_fs.readFileAsUtf8Sync("input/day06")
  ->Js.String2.split("\t")
  ->Array.map(Pervasives.int_of_string);

let distribute = banks => {
  // find the bank with most blocks && tie won with lowest-numbered bank
  let bank_in_full =
    banks->Array.reduceWithIndex(
      (0, 0),
      (acc, item, idx) => {
        let (_, prev_max) = acc;
        if (prev_max < item) {
          (idx, item);
        } else {
          acc;
        };
      },
    ); // (first_idx_of_max, value_of_max)

  // [0,0,0,1]
  // [1,1,1,1]
  // [1,1,0,0]
  // ---------
  // [2,2,1,2]

  // [0,0,0,1,1,1,1,1,1,1] => + [2,2,1,2]

  // 7 => 7 + idx + 1 => Array.make(10) => Array.mapWithIndex()
  let (idx_of_max, max) = bank_in_full;

  let length_of_queue = max + idx_of_max + 1;

  let _ = banks[idx_of_max] = 0;

  Array.make(length_of_queue, 0)
  ->Array.mapWithIndex((idx, _) =>
      idx_of_max < idx && idx < max + idx_of_max + 1 ? 1 : 0
    ) // [0,0,0,1,1,1,1,1,1,1]
  ->Array.reduceWithIndex(banks, (acc, item, idx) => {
      switch (banks[idx mod Array.length(banks)]) {
      | None => acc
      | Some(blocks) =>
        let _ = acc[idx mod Array.length(banks)] = blocks + item; // blocks + 0 or 1
        acc;
      }
    });
};

// TEST : distribute
// [|1, 2, 14, 3, 4|]->distribute->Js.log; // [4, 5, 2, 6, 7]
// [|5, 2, 0, 13, 1, 10, 10, 6, 6, 9, 8, 4, 15, 6, 13, 4|]->distribute->Js.log;
/*
 [
    6, 3,  1, 14, 2, 11,
   11, 7,  7, 10, 9,  5,
    0, 7, 14,  5
 ]
 */

let rec runner = (banks, history) => {
  let distributed_banks = distribute(Array.copy(banks));
  switch (history) {
  | None =>
    runner(
      distributed_banks,
      Some([|Array.copy(banks), distributed_banks|]),
    )
  | Some(past) =>
    let is_exists =
      Array.reduce(past, 0, (acc, item) => {
        item == distributed_banks ? acc + 1 : acc
      })
      > 0;

    if (!is_exists) {
      runner(
        distributed_banks,
        Some(Array.concat(past, [|distributed_banks|])),
      );
    } else {
      let matched_idx =
        Array.getIndexBy(past, item => item == distributed_banks);
      switch (matched_idx) {
      | None => (Array.length(past), Array.length(past))
      | Some(offset) => (Array.length(past), Array.length(past) - offset)
      };
    };
  };
};

let part1_2 = input->runner(None)->Js.log;

/*
 1. structural comparison == 👍
 2. runner()로 하려면 named argument와 optional을 어떻게 써야할까..?
 */
