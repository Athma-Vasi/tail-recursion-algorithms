// T(n) = O(n)
// S(n) = O(1)

let numberOfStringsAsSubstrings = (patterns: array<string>, word: string) => {
  let length = Array.length(patterns)

  let rec loop = (count: int, index: int) => {
    switch index === length {
    | true => count
    | false => {
        let pattern = switch patterns->Array.at(index) {
        | None => ""
        | Some(str) => str
        }

        word->String.includes(pattern) ? loop(count + 1, index + 1) : loop(count, index + 1)
      }
    }
  }

  loop(0, 0)
}

let p1 = ["a", "abc", "bc", "d"]
let w1 = "abc"
let r1 = numberOfStringsAsSubstrings(p1, w1)
Console.log2("[a,abc,bc,d] abc", r1)

let p2 = ["a", "b", "c"]
let w2 = "aaaaabbbbb"
let r2 = numberOfStringsAsSubstrings(p2, w2)
Console.log2("[a,b,c] aaaaabbbbb", r2)

let p3 = ["a", "a", "a"]
let w3 = "ab"
let r3 = numberOfStringsAsSubstrings(p3, w3)
Console.log2("[a,a,a] ab", r3)
