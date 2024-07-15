// T(n) = O(n)
// S(n) = O(n)

let longestSubstring3 = (str: string): int => {
  let length = String.length(str)

  let rec loop = (
    ~charsSet: Set.t<string>,
    ~longestSubstr: int,
    ~lowIndex: int,
    ~highIndex: int,
  ) => {
    switch highIndex === length - 1 {
    | true => longestSubstr
    | false => {
        let newHighIndex = highIndex + 1
        let newHighChar = switch str->String.get(newHighIndex) {
        | None => ""
        | Some(char) => char
        }

        switch charsSet->Set.has(newHighChar) {
        | true => loop(~charsSet, ~longestSubstr, ~lowIndex=lowIndex + 1, ~highIndex=newHighIndex)
        | false => {
            charsSet->Set.add(newHighChar)
            loop(~charsSet, ~longestSubstr=longestSubstr + 1, ~lowIndex, ~highIndex=newHighIndex)
          }
        }
      }
    }
  }

  let firstChar = switch str->String.get(0) {
  | None => ""
  | Some(char) => char
  }
  let secondChar = switch str->String.get(1) {
  | None => ""
  | Some(char) => char
  }
  let charsSet = Set.make()
  charsSet->Set.add(firstChar)
  charsSet->Set.add(secondChar)

  loop(~charsSet, ~longestSubstr=Set.size(charsSet), ~lowIndex=0, ~highIndex=1)
}

let s1 = "abcabcbb"
let r1 = longestSubstring3(s1)
Console.log2("abcabcbb", r1)

let s2 = "bbbbb"
let r2 = longestSubstring3(s2)
Console.log2("bbbbb", r2)
