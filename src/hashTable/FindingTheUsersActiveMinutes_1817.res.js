// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Caml_option from "rescript/lib/es6/caml_option.js";
import * as Core__Array from "@rescript/core/src/Core__Array.res.js";

function findingTheUsersActiveMinutes(logs, k) {
  var makeIdActionsTable = function (idActionsTable, _index) {
    while(true) {
      var index = _index;
      if (index === logs.length) {
        return idActionsTable;
      }
      var t = logs.at(index);
      var match = t !== undefined ? t : [
          -1,
          -1
        ];
      var id = match[0];
      var set = idActionsTable.get(id);
      var actions = set !== undefined ? Caml_option.valFromOption(set) : new Set();
      actions.add(match[1]);
      idActionsTable.set(id, actions);
      _index = index + 1 | 0;
      continue ;
    };
  };
  var makeActionsUsersCountTable = function (actionsUsersCountTable, idActionsTuples, _index) {
    while(true) {
      var index = _index;
      if (index === idActionsTuples.length) {
        return actionsUsersCountTable;
      }
      var t = idActionsTuples.at(index);
      var match = t !== undefined ? t : [
          -1,
          new Set()
        ];
      var size = match[1].size;
      var c = actionsUsersCountTable.get(size);
      var count = c !== undefined ? c + 1 | 0 : 1;
      actionsUsersCountTable.set(size, count);
      _index = index + 1 | 0;
      continue ;
    };
  };
  var idActionsTuples = Array.from(makeIdActionsTable(new Map(), 0).entries());
  var actionsUsersCountTable = makeActionsUsersCountTable(new Map(), idActionsTuples, 0);
  var _answers = Core__Array.make(k, 0);
  var actionsUsersCountTuples = Array.from(actionsUsersCountTable.entries());
  var _index = 0;
  while(true) {
    var index = _index;
    var answers = _answers;
    if (index === actionsUsersCountTuples.length) {
      return answers;
    }
    var t = actionsUsersCountTuples.at(index);
    var match = t !== undefined ? t : [
        -1,
        -1
      ];
    var count = match[1];
    var actions = match[0];
    _index = index + 1 | 0;
    _answers = answers.map((function(actions,count){
        return function (n, idx) {
          if ((idx + 1 | 0) === actions) {
            return count;
          } else {
            return n;
          }
        }
        }(actions,count)));
    continue ;
  };
}

var l1 = [
  [
    0,
    5
  ],
  [
    1,
    2
  ],
  [
    0,
    2
  ],
  [
    0,
    5
  ],
  [
    1,
    3
  ]
];

var r1 = findingTheUsersActiveMinutes(l1, 5);

console.log("r1: ", r1);

var l2 = [
  [
    1,
    1
  ],
  [
    2,
    2
  ],
  [
    2,
    3
  ]
];

var r2 = findingTheUsersActiveMinutes(l2, 4);

console.log("r2: ", r2);

var k1 = 5;

var k2 = 4;

export {
  findingTheUsersActiveMinutes ,
  l1 ,
  k1 ,
  r1 ,
  l2 ,
  k2 ,
  r2 ,
}
/* r1 Not a pure module */