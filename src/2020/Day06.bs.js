// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Belt_SetString = require("bs-platform/lib/js/belt_SetString.js");
var Caml_splice_call = require("bs-platform/lib/js/caml_splice_call.js");

var inputTest = Fs.readFileSync("input/2020/day06test", "utf8").trim().split("\n\n");

var input = Fs.readFileSync("input/2020/day06", "utf8").trim().split("\n\n");

console.log(Belt_Array.reduce(Belt_Array.map(input, (function (answer) {
                return Belt_SetString.size(Belt_SetString.fromArray(Belt_Array.keep(answer.split(""), (function ($$char) {
                                      return $$char !== "\n";
                                    }))));
              })), 0, (function (prim, prim$1) {
            return prim + prim$1 | 0;
          })));

console.log(Belt_Array.reduce(Belt_Array.map(Belt_Array.map(input, (function (answer) {
                    return Belt_Array.reduce(answer.split("\n"), "abcdefghijklmnopqrstuvwxyz", (function (dups, ans) {
                                  return Caml_splice_call.spliceObjApply("", "concat", [Belt_Array.keep(ans.split(""), (function ($$char) {
                                                      return dups.includes($$char);
                                                    }))]);
                                }));
                  })), (function (dups) {
                return dups.length;
              })), 0, (function (prim, prim$1) {
            return prim + prim$1 | 0;
          })));

exports.inputTest = inputTest;
exports.input = input;
/* inputTest Not a pure module */