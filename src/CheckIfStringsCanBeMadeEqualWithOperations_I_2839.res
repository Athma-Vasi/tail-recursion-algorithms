// T(n) = O(1)
// S(n) = O(1)

// https://leetcode.com/problems/check-if-strings-can-be-made-equal-with-operations-i/solutions/3992861/easy-c-o-1-check-if-characters-at-even-and-odd-indices-in-both-strings-match/
let checkIfStringsCanBeMadeEqualWithOperations_I = (s1: string, s2: string) => {
  let s1ZeroChar = s1->String.charAt(0)
  let s1FirstChar = s1->String.charAt(1)
  let s1SecondChar = s1->String.charAt(2)
  let s1ThirdChar = s1->String.charAt(3)

  let s2ZeroChar = s2->String.charAt(0)
  let s2FirstChar = s2->String.charAt(1)
  let s2SecondChar = s2->String.charAt(2)
  let s2ThirdChar = s2->String.charAt(3)

  let evenCheck =
    (s1ZeroChar === s2ZeroChar && s1SecondChar === s2SecondChar) ||
      (s1ZeroChar === s2SecondChar && s1SecondChar === s2ZeroChar)
  let oddCheck =
    (s1FirstChar === s2FirstChar && s1ThirdChar === s2ThirdChar) ||
      (s1FirstChar === s2ThirdChar && s1ThirdChar === s2FirstChar)

  evenCheck && oddCheck
}

let s1 = "abcd"
let s11 = "cdab"
let r1 = checkIfStringsCanBeMadeEqualWithOperations_I(s1, s11)
Console.log2("r1: ", r1) // true

let s2 = "abcd"
let s22 = "dacb"
let r2 = checkIfStringsCanBeMadeEqualWithOperations_I(s2, s22)
Console.log2("r2: ", r2) // false
