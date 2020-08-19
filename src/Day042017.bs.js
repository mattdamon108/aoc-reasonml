// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Belt_SetString = require("bs-platform/lib/js/belt_SetString.js");

var input = Belt_Array.map(Fs.readFileSync("input/day04", "utf8").split("\n"), (function (row) {
        return row.trim().split(" ");
      }));

function is_valid(arr) {
  var unique_length = Belt_SetString.toArray(Belt_SetString.fromArray(arr)).length;
  return arr.length === unique_length;
}

console.log(Belt_Array.reduce(Belt_Array.map(input, is_valid), 0, (function (acc, item) {
            if (item) {
              return acc + 1 | 0;
            } else {
              return acc;
            }
          })));

function is_valid_anagram(arr) {
  var converted_to_set = Belt_Array.map(arr, (function (item) {
          return Belt_SetString.fromArray(item.split(""));
        }));
  return Belt_Array.reduce(Belt_Array.map(converted_to_set, (function (pw) {
                    return Belt_Array.reduce(converted_to_set, -1, (function (acc, item) {
                                  if (Belt_SetString.eq(pw, item)) {
                                    return acc + 1 | 0;
                                  } else {
                                    return acc;
                                  }
                                }));
                  })), 0, (function (prim, prim$1) {
                return prim + prim$1 | 0;
              })) === 0;
}

console.log(Belt_Array.reduce(Belt_Array.map(input, (function (row) {
                if (is_valid_anagram(row)) {
                  return 1;
                } else {
                  return 0;
                }
              })), 0, (function (prim, prim$1) {
            return prim + prim$1 | 0;
          })));

var part1;

var part2;

exports.input = input;
exports.is_valid = is_valid;
exports.part1 = part1;
exports.is_valid_anagram = is_valid_anagram;
exports.part2 = part2;
/* input Not a pure module */