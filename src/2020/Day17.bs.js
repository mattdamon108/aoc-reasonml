// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Belt_Id = require("bs-platform/lib/js/belt_Id.js");
var Belt_Map = require("bs-platform/lib/js/belt_Map.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Caml_primitive = require("bs-platform/lib/js/caml_primitive.js");

var inputTest = Fs.readFileSync("input/2020/d17test", "utf8").trim().split("\n");

var input = Fs.readFileSync("input/2020/d17", "utf8").trim().split("\n");

var cmp = Caml_obj.caml_compare;

var ThreeDimCmp = Belt_Id.MakeComparable({
      cmp: cmp
    });

function parse(data) {
  return Belt_Array.reduceWithIndex(data, Belt_Map.make(ThreeDimCmp), (function (acc, row, idxY) {
                return Belt_Array.reduceWithIndex(row.split(""), acc, (function (ac, col, idxX) {
                              return Belt_Map.set(ac, [
                                          idxX - 1 | 0,
                                          idxY - 1 | 0,
                                          0
                                        ], col === "#" ? /* Active */0 : /* Inactive */1);
                            }));
              }));
}

function adjacents(param) {
  var z = param[2];
  var y = param[1];
  var x = param[0];
  var xs = Belt_Array.range(x - 1 | 0, x + 1 | 0);
  var ys = Belt_Array.range(y - 1 | 0, y + 1 | 0);
  var zs = Belt_Array.range(z - 1 | 0, z + 1 | 0);
  return Belt_Array.keep(Belt_Array.reduce(xs, [], (function (acc, xi) {
                    return Belt_Array.reduce(ys, acc, (function (ac, yi) {
                                  return Belt_Array.reduce(zs, ac, (function (a, zi) {
                                                return Belt_Array.concat(a, [[
                                                              xi,
                                                              yi,
                                                              zi
                                                            ]]);
                                              }));
                                }));
                  })), (function (i) {
                return Caml_obj.caml_compare(i, [
                            x,
                            y,
                            z
                          ]) !== 0;
              }));
}

function checkCube(map, pos, status) {
  var match = Belt_Array.reduce(adjacents(pos), [
        0,
        0
      ], (function (acc, cube) {
          var ci = acc[1];
          var ca = acc[0];
          var s = Belt_Map.get(map, cube);
          if (s !== undefined) {
            if (s === /* Active */0) {
              return [
                      ca + 1 | 0,
                      ci
                    ];
            } else {
              return [
                      ca,
                      ci + 1 | 0
                    ];
            }
          } else {
            return acc;
          }
        }));
  var countOfActive = match[0];
  if (status) {
    if (countOfActive === 3) {
      return /* Active */0;
    } else {
      return /* Inactive */1;
    }
  } else if (countOfActive === 2 || countOfActive === 3) {
    return /* Active */0;
  } else {
    return /* Inactive */1;
  }
}

function makeSurround(map) {
  var match = Belt_Map.reduce(map, [
        0,
        0,
        0
      ], (function (acc, k, v) {
          return [
                  Caml_primitive.caml_int_max(acc[0], k[0]),
                  Caml_primitive.caml_int_max(acc[1], k[1]),
                  Caml_primitive.caml_int_max(acc[2], k[2])
                ];
        }));
  var match$1 = Belt_Map.reduce(map, [
        Pervasives.max_int,
        Pervasives.max_int,
        Pervasives.max_int
      ], (function (acc, k, v) {
          return [
                  Caml_primitive.caml_int_min(acc[0], k[0]),
                  Caml_primitive.caml_int_min(acc[1], k[1]),
                  Caml_primitive.caml_int_min(acc[2], k[2])
                ];
        }));
  var xs = Belt_Array.range(match$1[0] - 1 | 0, match[0] + 1 | 0);
  var ys = Belt_Array.range(match$1[1] - 1 | 0, match[1] + 1 | 0);
  var zs = Belt_Array.range(match$1[2] - 1 | 0, match[2] + 1 | 0);
  return Belt_Array.reduce(xs, Belt_Map.make(ThreeDimCmp), (function (acc, xi) {
                return Belt_Array.reduce(ys, acc, (function (ac, yi) {
                              return Belt_Array.reduce(zs, ac, (function (a, zi) {
                                            return Belt_Map.set(a, [
                                                        xi,
                                                        yi,
                                                        zi
                                                      ], /* Inactive */1);
                                          }));
                            }));
              }));
}

function cycle(cubes, count) {
  var _map = cubes;
  var _n = 0;
  while(true) {
    var n = _n;
    var map = _map;
    if (n === count) {
      return map;
    }
    var surround = makeSurround(map);
    var newMap = Belt_Map.mapWithKey(surround, (function(map){
        return function (k, v) {
          var v$prime = Belt_Map.get(map, k);
          if (v$prime !== undefined) {
            return checkCube(map, k, v$prime);
          } else {
            return checkCube(map, k, v);
          }
        }
        }(map)));
    _n = n + 1 | 0;
    _map = newMap;
    continue ;
  };
}

function countActive(cubes) {
  return Belt_Map.reduce(cubes, 0, (function (acc, k, v) {
                if (v === /* Active */0) {
                  return acc + 1 | 0;
                } else {
                  return acc;
                }
              }));
}

console.log(countActive(cycle(parse(input), 6)));

var cmp$1 = Caml_obj.caml_compare;

var FourDimCmp = Belt_Id.MakeComparable({
      cmp: cmp$1
    });

function parse2(data) {
  return Belt_Array.reduceWithIndex(data, Belt_Map.make(FourDimCmp), (function (acc, row, idxY) {
                return Belt_Array.reduceWithIndex(row.split(""), acc, (function (ac, col, idxX) {
                              return Belt_Map.set(ac, [
                                          idxX - 1 | 0,
                                          idxY - 1 | 0,
                                          0,
                                          0
                                        ], col === "#" ? /* Active */0 : /* Inactive */1);
                            }));
              }));
}

function adjacents2(param) {
  var w = param[3];
  var z = param[2];
  var y = param[1];
  var x = param[0];
  var xs = Belt_Array.range(x - 1 | 0, x + 1 | 0);
  var ys = Belt_Array.range(y - 1 | 0, y + 1 | 0);
  var zs = Belt_Array.range(z - 1 | 0, z + 1 | 0);
  var ws = Belt_Array.range(w - 1 | 0, w + 1 | 0);
  return Belt_Array.keep(Belt_Array.reduce(xs, [], (function (accc, xi) {
                    return Belt_Array.reduce(ys, accc, (function (acc, yi) {
                                  return Belt_Array.reduce(zs, acc, (function (ac, zi) {
                                                return Belt_Array.reduce(ws, ac, (function (a, wi) {
                                                              return Belt_Array.concat(a, [[
                                                                            xi,
                                                                            yi,
                                                                            zi,
                                                                            wi
                                                                          ]]);
                                                            }));
                                              }));
                                }));
                  })), (function (i) {
                return Caml_obj.caml_compare(i, [
                            x,
                            y,
                            z,
                            w
                          ]) !== 0;
              }));
}

function checkCube2(map, pos, status) {
  var match = Belt_Array.reduce(adjacents2(pos), [
        0,
        0
      ], (function (acc, cube) {
          var ci = acc[1];
          var ca = acc[0];
          var s = Belt_Map.get(map, cube);
          if (s !== undefined) {
            if (s === /* Active */0) {
              return [
                      ca + 1 | 0,
                      ci
                    ];
            } else {
              return [
                      ca,
                      ci + 1 | 0
                    ];
            }
          } else {
            return acc;
          }
        }));
  var countOfActive = match[0];
  if (status) {
    if (countOfActive === 3) {
      return /* Active */0;
    } else {
      return /* Inactive */1;
    }
  } else if (countOfActive === 2 || countOfActive === 3) {
    return /* Active */0;
  } else {
    return /* Inactive */1;
  }
}

function makeSurround2(map) {
  var match = Belt_Map.reduce(map, [
        0,
        0,
        0,
        0
      ], (function (acc, k, v) {
          return [
                  Caml_primitive.caml_int_max(acc[0], k[0]),
                  Caml_primitive.caml_int_max(acc[1], k[1]),
                  Caml_primitive.caml_int_max(acc[2], k[2]),
                  Caml_primitive.caml_int_max(acc[3], k[3])
                ];
        }));
  var match$1 = Belt_Map.reduce(map, [
        Pervasives.max_int,
        Pervasives.max_int,
        Pervasives.max_int,
        Pervasives.max_int
      ], (function (acc, k, v) {
          return [
                  Caml_primitive.caml_int_min(acc[0], k[0]),
                  Caml_primitive.caml_int_min(acc[1], k[1]),
                  Caml_primitive.caml_int_min(acc[2], k[2]),
                  Caml_primitive.caml_int_min(acc[3], k[3])
                ];
        }));
  var xs = Belt_Array.range(match$1[0] - 1 | 0, match[0] + 1 | 0);
  var ys = Belt_Array.range(match$1[1] - 1 | 0, match[1] + 1 | 0);
  var zs = Belt_Array.range(match$1[2] - 1 | 0, match[2] + 1 | 0);
  var ws = Belt_Array.range(match$1[3] - 1 | 0, match[3] + 1 | 0);
  return Belt_Array.reduce(xs, Belt_Map.make(FourDimCmp), (function (accc, xi) {
                return Belt_Array.reduce(ys, accc, (function (acc, yi) {
                              return Belt_Array.reduce(zs, acc, (function (ac, zi) {
                                            return Belt_Array.reduce(ws, ac, (function (a, wi) {
                                                          return Belt_Map.set(a, [
                                                                      xi,
                                                                      yi,
                                                                      zi,
                                                                      wi
                                                                    ], /* Inactive */1);
                                                        }));
                                          }));
                            }));
              }));
}

function cycle2(cubes, count) {
  var _map = cubes;
  var _n = 0;
  while(true) {
    var n = _n;
    var map = _map;
    if (n === count) {
      return map;
    }
    var surround = makeSurround2(map);
    var newMap = Belt_Map.mapWithKey(surround, (function(map){
        return function (k, v) {
          var v$prime = Belt_Map.get(map, k);
          if (v$prime !== undefined) {
            return checkCube2(map, k, v$prime);
          } else {
            return checkCube2(map, k, v);
          }
        }
        }(map)));
    _n = n + 1 | 0;
    _map = newMap;
    continue ;
  };
}

console.log(countActive(cycle2(parse2(input), 6)));

exports.inputTest = inputTest;
exports.input = input;
exports.ThreeDimCmp = ThreeDimCmp;
exports.parse = parse;
exports.adjacents = adjacents;
exports.checkCube = checkCube;
exports.makeSurround = makeSurround;
exports.cycle = cycle;
exports.countActive = countActive;
exports.FourDimCmp = FourDimCmp;
exports.parse2 = parse2;
exports.adjacents2 = adjacents2;
exports.checkCube2 = checkCube2;
exports.makeSurround2 = makeSurround2;
exports.cycle2 = cycle2;
/* inputTest Not a pure module */