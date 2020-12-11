open Belt;

let factorial = n => {
  let rec compute = (m, answer) =>
    if (m === 0) {
      answer;
    } else {
      let newAnswer = answer * m;
      compute(m - 1, newAnswer);
    };

  compute(n, 1);
};

exception OutOfBound(string);
let combination = (n, r) =>
  if (n < r) {
    raise(OutOfBound("factorial error"));
  } else {
    factorial(n) / (factorial(r) * factorial(n - r));
  };

let frequencies = arr => {
  arr
  ->Array.reduce(
      Map.Int.empty,
      (acc, item) => {
        let freq = acc->Map.Int.get(item);
        switch (freq) {
        | Some(v) => acc->Map.Int.set(item, v + 1)
        | None => acc->Map.Int.set(item, 1)
        };
      },
    )
  ->Map.Int.toArray;
};

let partitionA = (arr, n, step) => {
  if (n <= 0 || step <= 0) {
    raise(OutOfBound("'n', 'step' should be a positive integer"));
  };
  let total = arr->Array.length;

  let rec divider = (count, sliced) => {
    let start = count * step;
    let end_ = start + n;
    if (end_ > total) {
      sliced;
    } else {
      divider(
        count + 1,
        sliced->Array.concat([|arr->Array.slice(~offset=start, ~len=n)|]),
      );
    };
  };

  divider(0, [||]);
};

let partitionAtoEnd = (arr, n, step) => {
  if (n <= 0 || step <= 0) {
    raise(OutOfBound("'n', 'step' should be a positive integer"));
  };
  let total = arr->Array.length;

  let rec divider = (count, sliced) => {
    let start = count * step;
    let end_ = start + n;
    if (end_ > total) {
      sliced->Array.concat([|arr->Array.slice(~offset=start, ~len=n)|]);
    } else {
      divider(
        count + 1,
        sliced->Array.concat([|arr->Array.slice(~offset=start, ~len=n)|]),
      );
    };
  };

  divider(0, [||]);
};
