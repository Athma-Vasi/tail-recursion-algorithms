// T(n) = O(n)
// S(n) = O(1)

let checkIfStringIsAcronymOfWords = (words: array<string>, s: string) => {
  let rec makeAcronym = (acronym: string, index: int) => {
    switch index === Array.length(words) {
    | true => acronym
    | false => {
        let word = switch words->Array.at(index) {
        | None => String.make()
        | Some(w) => w
        }
        let firstChar = word->String.charAt(0)

        makeAcronym(acronym->String.concat(firstChar), index + 1)
      }
    }
  }

  makeAcronym(String.make(), 0) === s
}

let w1 = ["alice", "bob", "charlie"]
let s1 = "abc"
let r1 = checkIfStringIsAcronymOfWords(w1, s1)
Console.log2("r1: ", r1) // true

let w2 = ["an", "apple"]
let s2 = "a"
let r2 = checkIfStringIsAcronymOfWords(w2, s2)
Console.log2("r2: ", r2) // false

let w3 = ["never", "gonna", "give", "up", "on", "you"]
let s3 = "ngguoy"
let r3 = checkIfStringIsAcronymOfWords(w3, s3)
Console.log2("r3: ", r3) // true
