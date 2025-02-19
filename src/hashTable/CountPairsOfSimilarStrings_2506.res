// T(n) = O(n)
// S(n) = O(n)
// @see https://leetcode.com/problems/count-pairs-of-similar-strings/solutions/2926549/easy-javascript-set-sort-and-map/

let countPairsOfSimilarStrings = (words: array<string>) => {
  let uniques = words->Array.reduce([], (accum, word) => {
    let unique =
      word
      ->String.split("")
      ->Array.reduce(Set.make(), (acc, char) => {
        acc->Set.add(char)
        acc
      })
      ->Set.values
      ->Array.fromIterator
      ->Array.toSorted((v1, v2) => String.compare(v1, v2))
      ->Array.join("")
    accum->Array.concat([unique])
  })

  let pairs = uniques->Array.reduce(Map.make(), (acc, unique) => {
    let count = acc->Map.get(unique)->Option.mapOr(1, u => u + 1)
    acc->Map.set(unique, count)
    acc
  })

  pairs
  ->Map.values
  ->Array.fromIterator
  ->Array.reduce(0, (acc, count) => acc + count * (count - 1) / 2)
}

let w1 = ["aba", "aabb", "abcd", "bac", "aabc"]
let r1 = countPairsOfSimilarStrings(w1)
Console.log2("r1: ", r1) // 2

let w2 = ["aabb", "ab", "ba"]
let r2 = countPairsOfSimilarStrings(w2)
Console.log2("r2: ", r2) // 3

let w3 = ["nba", "cba", "dba"]
let r3 = countPairsOfSimilarStrings(w3)
Console.log2("r3: ", r3) // 0
