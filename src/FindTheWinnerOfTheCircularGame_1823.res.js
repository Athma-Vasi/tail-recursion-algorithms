// Generated by ReScript, PLEASE EDIT WITH CARE


function findTheWinnerOfTheCircularGame(n, k) {
  var makeRange = function (start, end) {
    var _range = [];
    var _limit = start;
    while(true) {
      var limit = _limit;
      var range = _range;
      if (limit > end) {
        return range;
      }
      _limit = limit + 1 | 0;
      _range = range.concat([limit]);
      continue ;
    };
  };
  var removeNum = function (nums, index) {
    return nums.filter(function (_num, idx) {
                return idx !== index;
              });
  };
  var _remaining = makeRange(1, n);
  var _startIndex = 0;
  while(true) {
    var startIndex = _startIndex;
    var remaining = _remaining;
    var length = remaining.length;
    if (length <= 1) {
      var p = remaining.at(0);
      if (p !== undefined) {
        return p;
      } else {
        return 0;
      }
    }
    var newIndex = ((startIndex + k | 0) - 1 | 0) >= length ? ((startIndex + k | 0) - 1 | 0) % length : (startIndex + k | 0) - 1 | 0;
    var removed = removeNum(remaining, newIndex | 0);
    _startIndex = newIndex | 0;
    _remaining = removed;
    continue ;
  };
}

var r1 = findTheWinnerOfTheCircularGame(5, 2);

console.log("r1: ", r1);

var r2 = findTheWinnerOfTheCircularGame(6, 5);

console.log("r2: ", r2);

var n1 = 5;

var k1 = 2;

var n2 = 6;

var k2 = 5;

export {
  findTheWinnerOfTheCircularGame ,
  n1 ,
  k1 ,
  r1 ,
  n2 ,
  k2 ,
  r2 ,
}
/* r1 Not a pure module */