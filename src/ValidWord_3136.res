// T(n) = O(n)
// S(n) = O(n)

let validWord = (word: string) => {
  let letters = "bcdfghklmnpqrstvwxyz"
  let (lowerLettersSet, upperLettersSet) =
    letters
    ->String.split("")
    ->Array.reduce((Set.make(), Set.make()), (sets, char) => {
      let (lower, upper) = sets
      lower->Set.add(char)
      upper->Set.add(String.toUpperCase(char))
      (lower, upper)
    })

  let (lowerVowelsSet, upperVowelsSet) =
    "aeiou"
    ->String.split("")
    ->Array.reduce((Set.make(), Set.make()), (sets, char) => {
      let (lower, upper) = sets
      lower->Set.add(char)
      upper->Set.add(String.toUpperCase(char))
      (lower, upper)
    })

  let specialsSet =
    "@#$"
    ->String.split("")
    ->Array.reduce(Set.make(), (set, char) => {
      set->Set.add(char)
      set
    })

  let wordLength = String.length(word)

  let rec loop = (
    isVowels: Set.t<bool>,
    isConsonants: Set.t<bool>,
    isInvalids: Set.t<bool>,
    index: int,
  ) => {
    switch wordLength < 3 {
    | true => false
    | false =>
      switch index === wordLength {
      | true => isVowels->Set.has(true) && isConsonants->Set.has(true) && !Set.has(isInvalids, true)
      | false => {
          let char = word->String.charAt(index)
          let isVowel = lowerVowelsSet->Set.has(char) || upperVowelsSet->Set.has(char)
          let isConsonant = lowerLettersSet->Set.has(char) || upperLettersSet->Set.has(char)
          let isInvalid = specialsSet->Set.has(char)

          isVowels->Set.add(isVowel)
          isConsonants->Set.add(isConsonant)
          isInvalids->Set.add(isInvalid)

          loop(isVowels, isConsonants, isInvalids, index + 1)
        }
      }
    }
  }

  loop(Set.make(), Set.make(), Set.make(), 0)
}

let w1 = "234Adas"
let r1 = validWord(w1)
Console.log2("r1: ", r1) // true

let w2 = "b3"
let r2 = validWord(w2)
Console.log2("r2: ", r2) // false

let w3 = "a3$e"
let r3 = validWord(w3)
Console.log2("r3: ", r3) // false
