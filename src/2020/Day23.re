open Belt;

module type Deque = {
  type t('a) = array('a);
  let make: array('a) => t('a);
  let isEmpty: t('a) => bool;
  let addFront: (t('a), 'a) => t('a);
  let addFrontMany: (t('a), array('a)) => t('a);
  let addBack: (t('a), 'a) => t('a);
  let removeFront: t('a) => t('a);
  let removeBack: t('a) => t('a);
  let peekFront: t('a) => option('a);
  let peekBack: t('a) => option('a);
};

module Deque: Deque = {
  type t('a) = array('a);
  let make = arr => arr;
  let isEmpty = dq => dq->Array.length === 0;
  let addFront = (dq, i) => [|i|]->Array.concat(dq);
  let addFrontMany = (dq, cups) => cups->Array.concat(dq);
  let addBack = (dq, i) => dq->Array.concat([|i|]);
  let removeFront = dq => dq->Array.sliceToEnd(1);
  let removeBack = dq => {
    let length = dq->Array.length;
    dq->Array.slice(~offset=0, ~len=length - 1);
  };
  let peekFront = dq => dq->Array.get(0);
  let peekBack = dq => {
    let length = dq->Array.length;
    dq->Array.get(length - 1);
  };
};

let inputTest = "389125467";
let input = "871369452";

exception UndefinedMax;

module type CrabCups = {
  type cup = int;
  type cups = array(cup);
  let parse: (string, int) => array(cup);
  let target: (cups, cup, cup, cup, cup) => cup;
  let move: (array(cup), cup, int) => array(cup);
};

module CrabCups: CrabCups = {
  type cup = int;

  type cups = array(cup);

  let parse = (data, addition) => {
    let splitted = data->Js.String2.split("")->Array.map(int_of_string);
    let maxSplitted = splitted->Array.reduce(0, max);
    let additional = Array.range(maxSplitted + 1, addition);
    let total = splitted->Array.concat(additional);
    let maxTotal = total->Array.reduce(0, max);
    let next = Array.make(maxTotal + 1, 0);

    total->Array.forEachWithIndex((idx, cup) => {
      switch (total->Array.get(idx + 1)) {
      | Some(nv) => next->Array.set(cup, nv)->ignore
      | None => next->Array.set(cup, total->Array.getExn(0))->ignore
      }
    });

    next;
  };

  let target = (next, cur, pick1, pick2, pick3) => {
    let rec seek = i =>
      if (cur - i > 0) {
        if (cur - i !== pick1 && cur - i !== pick2 && cur - i !== pick3) {
          let rec seek2 = (v, count) => {
            let n = next->Array.getExn(v);
            if (count > next->Array.length) {
              seek(i + 1);
            } else if (n === cur - i) {
              n;
            } else {
              seek2(n, count + 1);
            };
          };

          seek2(pick3, 0);
        } else {
          seek(i + 1);
        };
      } else {
        let length = next->Array.length;
        let max =
          [|length - 1, length - 2, length - 3, length - 4|]
          ->Array.keep(m => {
              !(m === cur || m === pick1 || m === pick2 || m === pick3)
            })
          ->Array.reduce(0, max);

        let rec seek2 = (v, count) => {
          let n = next->Array.getExn(v);
          if (count > next->Array.length) {
            seek(i + 1);
          } else if (n === max) {
            n;
          } else {
            seek2(n, count + 1);
          };
        };

        seek2(pick3, 0);
      };

    seek(1);
  };

  let move = (next, first, count) => {
    let rec m = (nx, cur, c) =>
      if (c === 0) {
        nx;
      } else {
        c mod 100 === 0 ? Js.log(c) : ();
        let pick1 = nx->Array.getExn(cur);
        let pick2 = nx->Array.getExn(pick1);
        let pick3 = nx->Array.getExn(pick2);
        let target = target(nx, cur, pick1, pick2, pick3);
        let nextTarget = nx->Array.getExn(target);
        let nextPick3 = nx->Array.getExn(pick3);
        nx->Array.set(target, pick1)->ignore;
        nx->Array.set(pick3, nextTarget)->ignore;
        nx->Array.set(cur, nextPick3)->ignore;
        m(nx, nx->Array.getExn(cur), c - 1);
      };

    m(next, first, count);
  };
};

let rec part1 = (next, after, acc) => {
  let nextValue = next->Array.getExn(after);
  acc->Array.some(a => a === nextValue)
    ? acc->Array.joinWith("", string_of_int)
    : part1(next, nextValue, acc->Array.concat([|nextValue|]));
};

let part2 = next => {
  let nextOne = next->Array.getExn(1);
  nextOne * next->Array.getExn(nextOne);
};

// part1
input->CrabCups.parse(9)->CrabCups.move(8, 100)->part1(1, [||])->Js.log; // 1, 2, 8, 7, 9, 3, 6, 5, 4 => 28793654

// part2
input
->CrabCups.parse(1000000)
->CrabCups.move(8, 10000000)
->part2
->Js.log;
