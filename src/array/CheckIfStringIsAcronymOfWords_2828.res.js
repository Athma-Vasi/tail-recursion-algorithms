// Generated by ReScript, PLEASE EDIT WITH CARE


function checkIfStringIsAcronymOfWords(words, s) {
  var makeAcronym = function (_acronym, _index) {
    while(true) {
      var index = _index;
      var acronym = _acronym;
      if (index === words.length) {
        return acronym;
      }
      var w = words.at(index);
      var word = w !== undefined ? w : String();
      var firstChar = word.charAt(0);
      _index = index + 1 | 0;
      _acronym = acronym.concat(firstChar);
      continue ;
    };
  };
  return makeAcronym(String(), 0) === s;
}

var w1 = [
  "alice",
  "bob",
  "charlie"
];

var s1 = "abc";

var r1 = checkIfStringIsAcronymOfWords(w1, s1);

console.log("r1: ", r1);

var w2 = [
  "an",
  "apple"
];

var s2 = "a";

var r2 = checkIfStringIsAcronymOfWords(w2, s2);

console.log("r2: ", r2);

var w3 = [
  "never",
  "gonna",
  "give",
  "up",
  "on",
  "you"
];

var s3 = "ngguoy";

var r3 = checkIfStringIsAcronymOfWords(w3, s3);

console.log("r3: ", r3);

export {
  checkIfStringIsAcronymOfWords ,
  w1 ,
  s1 ,
  r1 ,
  w2 ,
  s2 ,
  r2 ,
  w3 ,
  s3 ,
  r3 ,
}
/* r1 Not a pure module */