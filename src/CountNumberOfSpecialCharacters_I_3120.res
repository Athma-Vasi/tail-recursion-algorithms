// T(n) = O(n)
// S(n) = O(n)

let countNumberOfSpecialCharacters_I = (word: string) => {
  let alphabet =
    "abcdefghijklmnopqrstuvwxyz"
    ->String.split("")
    ->Array.reduce(Set.make(), (set, char) => {
      set->Set.add(char)
      set
    })

  let rec loop = (count: int, lowercaseSet: Set.t<string>, index: int) => {
    switch index === String.length(word) {
    | true => count
    | false => {
        let char = word->String.charAt(index)

        switch alphabet->Set.has(char) {
        | true => {
            lowercaseSet->Set.add(char)
            loop(count, lowercaseSet, index + 1)
          }
        | false =>
          loop(
            lowercaseSet->Set.has(String.toLowerCase(char)) ? count + 1 : count,
            lowercaseSet,
            index + 1,
          )
        }
      }
    }
  }

  loop(0, Set.make(), 0)
}

let w1 = "aaAbcBC"
let r1 = countNumberOfSpecialCharacters_I(w1)
Console.log2("r1: ", r1) // 3

let w2 = "abc"
let r2 = countNumberOfSpecialCharacters_I(w2)
Console.log2("r2: ", r2) // 0

let w3 = "abBCab"
let r3 = countNumberOfSpecialCharacters_I(w3)
Console.log2("r3: ", r3) // 1
