// T(n) = O(n)
// S(n) = O(n)

let existenceOfSubstringInStringAndItsReverse = (str: string) => {
  let strLength = String.length(str)

  let rec makeSubstringSet = (substrSet: Set.t<string>, str: string, length: int, index: int) => {
    switch index === strLength - length + 1 {
    | true => substrSet
    | false => {
        let substr = str->String.slice(~start=index, ~end=index + length)
        substrSet->Set.add(substr)
        makeSubstringSet(substrSet, str, length, index + 1)
      }
    }
  }

  let rec reverseString = (reversed: string, str: string, index: int) => {
    switch index < 0 {
    | true => reversed
    | false => {
        let char = str->String.charAt(index)
        reverseString(reversed->String.concat(char), str, index - 1)
      }
    }
  }

  let substringSet = makeSubstringSet(Set.make(), str, 2, 0)
  let reversed = reverseString(String.make(), str, strLength - 1)
  let reversedSubstringSet = makeSubstringSet(Set.make(), reversed, 2, 0)

  let checkIntersection = (
    isIntersection: bool,
    substringSet: Set.t<string>,
    reversedSubstringSet: Set.t<string>,
  ) => {
    substringSet
    ->Set.values
    ->Core__Iterator.toArray
    ->Array.reduce(isIntersection, (result, substring) =>
      reversedSubstringSet->Set.has(substring) ? true : result
    )
  }

  checkIntersection(false, substringSet, reversedSubstringSet)
}

let s1 = "leetcode"
let r1 = existenceOfSubstringInStringAndItsReverse(s1)
Console.log2("r1: ", r1) // true

let s2 = "abcba"
let r2 = existenceOfSubstringInStringAndItsReverse(s2)
Console.log2("r2: ", r2) // true

let s3 = "abcd"
let r3 = existenceOfSubstringInStringAndItsReverse(s3)
Console.log2("r3: ", r3) // false
