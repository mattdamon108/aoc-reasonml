open Belt;

let inputTest =
  Node_fs.readFileAsUtf8Sync("input/2020/d22test")
  ->Js.String2.trim
  ->Js.String2.split("\n\n");

let input =
  Node_fs.readFileAsUtf8Sync("input/2020/d22")
  ->Js.String2.trim
  ->Js.String2.split("\n\n");

let parse = lines => {
  lines
  ->Js.String2.split("\n")
  ->Array.sliceToEnd(1)
  ->Array.map(i => i->int_of_string);
};

module type CrabCombat = {
  type player =
    | P1
    | P2;
  type deck = array(int);
  let loadDeck: array(string) => (deck, deck);
  let score: ((deck, deck, player)) => int;
  let play: ((deck, deck)) => (deck, deck, player);
  let playRecursive: ((deck, deck)) => (deck, deck, player);
};

module CrabCombat: CrabCombat = {
  type player =
    | P1
    | P2;

  type deck = array(int);

  let loadDeck = data => {
    let deckP1 = data->Array.getExn(0)->parse;
    let deckP2 = data->Array.getExn(1)->parse;
    (deckP1, deckP2);
  };

  let score = ((deckP1, deckP2, winner)) => {
    switch (winner) {
    | P1 =>
      deckP1
      ->Array.reverse
      ->Array.reduceWithIndex(0, (acc, card, idx) => {acc + (idx + 1) * card})
    | P2 =>
      deckP2
      ->Array.reverse
      ->Array.reduceWithIndex(0, (acc, card, idx) => {acc + (idx + 1) * card})
    };
  };

  let play = ((deckP1, deckP2)) => {
    let rec roll = (dp1, dp2) => {
      let havingP1 = dp1->Array.length;
      let havingP2 = dp2->Array.length;

      if (havingP1 === 0 || havingP2 === 0) {
        (dp1, dp2, havingP1 === 0 ? P2 : P1);
      } else {
        let drawP1 = dp1->Array.getExn(0);
        let deckP1AfterDraw = dp1->Array.sliceToEnd(1);
        let drawP2 = dp2->Array.getExn(0);
        let deckP2AfterDraw = dp2->Array.sliceToEnd(1);

        drawP1 > drawP2
          ? {
            let newDeckP1 = deckP1AfterDraw->Array.concat([|drawP1, drawP2|]);
            roll(newDeckP1, deckP2AfterDraw);
          }
          : {
            let newDeckP2 = deckP2AfterDraw->Array.concat([|drawP2, drawP1|]);
            roll(deckP1AfterDraw, newDeckP2);
          };
      };
    };

    roll(deckP1, deckP2);
  };

  module DeckCmp =
    Id.MakeComparable({
      type t = array(int);
      let cmp = (a, b) => Pervasives.compare(a, b);
    });

  let playRecursive = ((deckP1, deckP2)) => {
    let rec topRoll = (deckP1', deckP2') => {
      let rec roll = (dp1, dp2, h1, h2) => {
        let havingP1 = dp1->Array.length;
        let havingP2 = dp2->Array.length;

        if (havingP1 === 0 || havingP2 === 0) {
          (dp1, dp2, havingP1 === 0 ? P2 : P1);
        } else {
          switch (h1->Map.get(dp1), h2->Map.get(dp2)) {
          | (Some(_), Some(_)) => (dp1, dp2, P1)
          | _ =>
            let newH1 = h1->Map.set(dp1, 0);
            let newH2 = h2->Map.set(dp2, 0);

            let drawP1 = dp1->Array.getExn(0);
            let deckP1AfterDraw = dp1->Array.sliceToEnd(1);
            let drawP2 = dp2->Array.getExn(0);
            let deckP2AfterDraw = dp2->Array.sliceToEnd(1);

            if (deckP1AfterDraw->Array.length >= drawP1
                && deckP2AfterDraw->Array.length >= drawP2) {
              let (d1, d2, winner) =
                topRoll(
                  deckP1AfterDraw->Array.slice(~offset=0, ~len=drawP1),
                  deckP2AfterDraw->Array.slice(~offset=0, ~len=drawP2),
                );
              switch (winner) {
              | P1 =>
                let newDeckP1 =
                  deckP1AfterDraw->Array.concat([|drawP1, drawP2|]);
                roll(newDeckP1, deckP2AfterDraw, newH1, newH2);
              | P2 =>
                let newDeckP2 =
                  deckP2AfterDraw->Array.concat([|drawP2, drawP1|]);
                roll(deckP1AfterDraw, newDeckP2, newH1, newH2);
              };
            } else {
              drawP1 > drawP2
                ? {
                  let newDeckP1 =
                    deckP1AfterDraw->Array.concat([|drawP1, drawP2|]);
                  roll(newDeckP1, deckP2AfterDraw, newH1, newH2);
                }
                : {
                  let newDeckP2 =
                    deckP2AfterDraw->Array.concat([|drawP2, drawP1|]);
                  roll(deckP1AfterDraw, newDeckP2, newH1, newH2);
                };
            };
          };
        };
      };
      let history1 = Map.make(~id=(module DeckCmp));
      let history2 = Map.make(~id=(module DeckCmp));

      roll(deckP1', deckP2', history1, history2);
    };

    topRoll(deckP1, deckP2);
  };
};

module Solve = {
  open CrabCombat;
  let part1 = data => data->loadDeck->play->score->Js.log;
  let part2 = data => data->loadDeck->playRecursive->score->Js.log;
};

Solve.part1(input);
Solve.part2(input);
