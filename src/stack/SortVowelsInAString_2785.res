// T(n) = O(n)
// S(n) = O(n)

let sortVowelsInAString = (s: string) => {
  let (upperVowels, lowerVowels) =
    "aeiou"
    ->String.split("")
    ->Array.reduce((Set.make(), Set.make()), (acc, char) => {
      let (upper, lower) = acc
      upper->Set.add(String.toUpperCase(char))
      lower->Set.add(char)
      (upper, lower)
    })

  let mergeStacks = (vowelsStack, consonantsStack) => {
    let rec merge = (merged: string, slicedVowels: string, index: int) => {
      switch index === String.length(consonantsStack) {
      | true => merged
      | false => {
          let char = consonantsStack->String.charAt(index)

          switch char {
          | "&" =>
            merge(
              merged->String.concat(slicedVowels->String.charAt(0)),
              slicedVowels->String.sliceToEnd(~start=1),
              index + 1,
            )
          | _ => merge(merged->String.concat(char), slicedVowels, index + 1)
          }
        }
      }
    }

    merge(String.make(), vowelsStack, 0)
  }

  let rec makeStacks = (vowelsStack: string, consonantsStack: string, index: int) => {
    switch index === String.length(s) {
    | true =>
      mergeStacks(
        vowelsStack
        ->String.split("")
        ->Array.toSorted((c1, c2) => String.compare(c1, c2))
        ->Array.reduce(String.make(), (acc, char) => acc->String.concat(char)),
        consonantsStack,
      )
    | false => {
        let char = s->String.charAt(index)
        let isVowel = upperVowels->Set.has(char) || lowerVowels->Set.has(char)

        isVowel
          ? makeStacks(
              vowelsStack->String.concat(char),
              consonantsStack->String.concat("&"), // placeholder to be substituted
              index + 1,
            )
          : makeStacks(vowelsStack, consonantsStack->String.concat(char), index + 1)
      }
    }
  }

  makeStacks(String.make(), String.make(), 0)
}

let s1 = "lEetcOde"
let r1 = sortVowelsInAString(s1)
Console.log2("r1: ", r1) // "lEOtcede"

let s2 = "lYmpH"
let r2 = sortVowelsInAString(s2)
Console.log2("r2: ", r2) // "lYmpH"
