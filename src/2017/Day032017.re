/*
 * Advent of Code 2017 Day03
 */
open Belt;

let input = 347991;

let rec find_square_by = (step, point) => {
  switch (step) {
  | 0 => find_square_by(1, point)
  | _ =>
    if (step * step < point) {
      find_square_by(step + 2, point);
    } else {
      step;
    }
  };
};

let compute_distance = (step, point) => {
  let distance_from_apex = step - 1;
  let offset = (step * step - point) mod (step - 1);
  let half = step / 2 + 1;
  if (offset < half) {
    distance_from_apex - offset;
  } else {
    distance_from_apex - (step - offset - 1);
  };
};

let part1 = find_square_by(0, input)->compute_distance(input)->Js.log;

// sequence of move
// R, U, L, D, R, U, L, D, R, U

// adjacent
// (-1, -1) (0, -1) (1, -1)
// (-1, 0)  (0, 0)  (1, 0)
// (-1, 1) (0, 1) (1, 1)

let isAdjacent = (adj_x, adj_y, pos_x, pos_y) =>
  if (adj_x == pos_x + 1 && adj_y == pos_y) {
    true;
  } else if (adj_x == pos_x + 1 && adj_y == pos_y - 1) {
    true;
  } else if (adj_x == pos_x && adj_y == pos_y - 1) {
    true;
  } else if (adj_x == pos_x - 1 && adj_y == pos_y - 1) {
    true;
  } else if (adj_x == pos_x - 1 && adj_y == pos_y) {
    true;
  } else if (adj_x == pos_x - 1 && adj_y == pos_y + 1) {
    true;
  } else if (adj_x == pos_x && adj_y == pos_y + 1) {
    true;
  } else if (adj_x == pos_x + 1 && adj_y == pos_y + 1) {
    true;
  } else {
    false;
  };

let sum_of_adjacent = (moves, pos_x, pos_y) => {
  Array.reduce(
    moves,
    0,
    (acc, item) => {
      let (adj_x, adj_y, value) = item;
      if (isAdjacent(adj_x, adj_y, pos_x, pos_y)) {
        acc + value;
      } else {
        acc;
      };
    },
  );
};

type direction =
  | Right
  | Up
  | Left
  | Down;

let rec walker = (input, moves, x, y, prev_dir) => {
  let value = sum_of_adjacent(moves, x, y);
  let new_moves = Array.concat(moves, [|(x, y, value)|]);
  if (value < input) {
    switch (prev_dir) {
    | None => walker(input, [|(0, 0, 1)|], 1, 0, Some(Right))
    | Some(Right) =>
      // peek Right more then change the direction to Up
      if (sum_of_adjacent(new_moves, x + 1, y) == value) {
        walker(input, new_moves, x, y - 1, Some(Up));
      } else {
        walker(input, new_moves, x + 1, y, Some(Right));
      }
    | Some(Up) =>
      // peek Up more then change the direction to Left
      if (sum_of_adjacent(new_moves, x, y - 1) == value) {
        walker(input, new_moves, x - 1, y, Some(Left));
      } else {
        walker(input, new_moves, x, y - 1, Some(Up));
      }
    | Some(Left) =>
      // peek Left more then change the direction to Down
      if (sum_of_adjacent(new_moves, x - 1, y) == value) {
        walker(input, new_moves, x, y + 1, Some(Down));
      } else {
        walker(input, new_moves, x - 1, y, Some(Left));
      }
    | Some(Down) =>
      // peek Down more then change the direction to Right
      if (sum_of_adjacent(new_moves, x, y + 1) == value) {
        walker(input, new_moves, x + 1, y, Some(Right));
      } else {
        walker(input, new_moves, x, y + 1, Some(Down));
      }
    };
  } else {
    value;
  };
};

let part2 = input->walker([||], 0, 0, None)->Js.log;
