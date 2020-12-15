open Belt;

let inputTest = "0,3,6";
let input = "9,19,1,6,0,5,4";

module FloatCmp =
  Id.MakeComparable({
    type t = float;
    let cmp = (a, b) => Pervasives.compare(a, b);
  });

let numbers =
  Map.make(~id=(module FloatCmp))
  ->Map.set(9.0, 1.0)
  ->Map.set(19.0, 2.0)
  ->Map.set(1.0, 3.0)
  ->Map.set(6.0, 4.0)
  ->Map.set(0.0, 5.0)
  ->Map.set(5.0, 6.0);

let play = (map, nth, l) => {
  let rec run = (m, n, last) => {
    let spokenBefore = m->Map.get(last);
    let speakingNumber =
      switch (spokenBefore) {
      | Some(t) => n -. 1.0 -. t
      | None => 0.0
      };

    // debug
    if (mod_float(n, 100000.0) === 0.0) {
      n->Js.log2("...");
    };

    if (n === 30000000.0) {
      speakingNumber;
    } else {
      let newMap = m->Map.set(last, n -. 1.0);
      run(newMap, n +. 1.0, speakingNumber);
    };
  };

  run(map, nth, l);
};

// numbers에는 시작값들 중 마지막 값을 빼고 key: 값, value는 turn
// nth는 시작값들 이후 turn
// l은 시작값들의 마지막 값
numbers->play(8.0, 4.0)->Js.log;
