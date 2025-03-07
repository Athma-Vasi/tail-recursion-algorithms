// Generated by ReScript, PLEASE EDIT WITH CARE


function circularSentence(sentence) {
  var firstChar = sentence.charAt(0);
  var lastChar = sentence.charAt(sentence.length - 1 | 0);
  var words = sentence.split(" ");
  var c = words.at(0);
  var firstWord = c !== undefined ? c : "";
  var firstWordLastChar = firstWord.charAt(firstWord.length - 1 | 0);
  if (firstChar === lastChar) {
    var _prevLast = firstWordLastChar;
    var _words = words.slice(1, words.length);
    while(true) {
      var words$1 = _words;
      var prevLast = _prevLast;
      if (words$1.length === 0) {
        return true;
      }
      var c$1 = words$1.at(0);
      var word = c$1 !== undefined ? c$1 : "";
      var first = word.charAt(0);
      var currLast = word.charAt(word.length - 1 | 0);
      if (prevLast !== first) {
        return false;
      }
      _words = words$1.slice(1, words$1.length);
      _prevLast = currLast;
      continue ;
    };
  } else {
    return false;
  }
}

var s1 = "leetcode exercises sound delightful";

var r1 = circularSentence(s1);

console.log("r1: ", r1);

var s2 = "sound delightful leetcode exercises";

var r2 = circularSentence(s2);

console.log("r2: ", r2);

var s3 = "Leetcode is cool";

var r3 = circularSentence(s3);

console.log("r3: ", r3);

export {
  circularSentence ,
  s1 ,
  r1 ,
  s2 ,
  r2 ,
  s3 ,
  r3 ,
}
/* r1 Not a pure module */
