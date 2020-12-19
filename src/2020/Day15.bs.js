// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Belt_Id = require("bs-platform/lib/js/belt_Id.js");
var Belt_Map = require("bs-platform/lib/js/belt_Map.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");

var cmp = Caml_obj.caml_compare;

var FloatCmp = Belt_Id.MakeComparable({
      cmp: cmp
    });

var numbers = Belt_Map.set(Belt_Map.set(Belt_Map.set(Belt_Map.set(Belt_Map.set(Belt_Map.set(Belt_Map.make(FloatCmp), 9.0, 1.0), 19.0, 2.0), 1.0, 3.0), 6.0, 4.0), 0.0, 5.0), 5.0, 6.0);

function play(map, nth, l) {
  var _m = map;
  var _n = nth;
  var _last = l;
  while(true) {
    var last = _last;
    var n = _n;
    var m = _m;
    var spokenBefore = Belt_Map.get(m, last);
    var speakingNumber = spokenBefore !== undefined ? n - 1.0 - spokenBefore : 0.0;
    if (n % 100000.0 === 0.0) {
      console.log(n, "...");
    }
    if (n === 30000000.0) {
      return speakingNumber;
    }
    var newMap = Belt_Map.set(m, last, n - 1.0);
    _last = speakingNumber;
    _n = n + 1.0;
    _m = newMap;
    continue ;
  };
}

console.log(play(numbers, 8.0, 4.0));

var inputTest = "0,3,6";

var input = "9,19,1,6,0,5,4";

exports.inputTest = inputTest;
exports.input = input;
exports.FloatCmp = FloatCmp;
exports.numbers = numbers;
exports.play = play;
/* FloatCmp Not a pure module */