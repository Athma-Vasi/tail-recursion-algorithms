// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Core__Array from "@rescript/core/src/Core__Array.res.js";

function keyboardRow(words) {
  var firstRowSet = Core__Array.reduce("qwertyuiop".split(""), new Set(), (function (setAcc, $$char) {
          setAcc.add($$char);
          return setAcc;
        }));
  var secondRowSet = Core__Array.reduce("asdfghjkl".split(""), new Set(), (function (setAcc, $$char) {
          setAcc.add($$char);
          return setAcc;
        }));
  var thirdRowSet = Core__Array.reduce("zxcvbnm".split(""), new Set(), (function (setAcc, $$char) {
          setAcc.add($$char);
          return setAcc;
        }));
  var _result = [];
  var _wordsIndex = 0;
  while(true) {
    var wordsIndex = _wordsIndex;
    var result = _result;
    var w = words.at(wordsIndex);
    var word = w !== undefined ? w : "";
    var wordLength = word.length;
    var wordLoop = (function(word,wordLength){
    return function wordLoop(_firstRowStack, _secondRowStack, _thirdRowStack, _wordIndex) {
      while(true) {
        var wordIndex = _wordIndex;
        var thirdRowStack = _thirdRowStack;
        var secondRowStack = _secondRowStack;
        var firstRowStack = _firstRowStack;
        var $$char = word.charAt(wordIndex).toLowerCase();
        if (wordIndex === wordLength) {
          return [
                  firstRowStack,
                  secondRowStack,
                  thirdRowStack
                ];
        }
        _wordIndex = wordIndex + 1 | 0;
        _thirdRowStack = thirdRowSet.has($$char) ? thirdRowStack.concat($$char) : thirdRowStack;
        _secondRowStack = secondRowSet.has($$char) ? secondRowStack.concat($$char) : secondRowStack;
        _firstRowStack = firstRowSet.has($$char) ? firstRowStack.concat($$char) : firstRowStack;
        continue ;
      };
    }
    }(word,wordLength));
    var match = wordLoop("", "", "", 0);
    if (wordsIndex === words.length) {
      return result;
    }
    _wordsIndex = wordsIndex + 1 | 0;
    _result = match[0].length === wordLength || match[1].length === wordLength || match[2].length === wordLength ? result.concat([word]) : result;
    continue ;
  };
}

var w1 = [
  "Hello",
  "Alaska",
  "Dad",
  "Peace"
];

var r1 = keyboardRow(w1);

console.log("r1: ", r1);

var w2 = ["omk"];

var r2 = keyboardRow(w2);

console.log("r2: ", r2);

var w3 = [
  "adsdf",
  "sfd"
];

var r3 = keyboardRow(w3);

console.log("r3: ", r3);

export {
  keyboardRow ,
  w1 ,
  r1 ,
  w2 ,
  r2 ,
  w3 ,
  r3 ,
}
/* r1 Not a pure module */
