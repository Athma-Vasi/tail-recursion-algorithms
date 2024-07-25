// T(n) = O(n)
// S(n) = O(n)
let removeAllAdjDupesInString = (str: string) => {
  let strLength = String.length(str)

  let rec loop = (charStack: string, index: int) => {
    let charStackLength = String.length(charStack)

    switch index === strLength {
    | true => charStack
    | false => {
        let currentChar = str->String.charAt(index)
        let previousChar = charStack->String.charAt(charStackLength - 1)

        switch currentChar === previousChar {
        | true => loop(charStack->String.slice(~start=0, ~end=charStackLength - 1), index + 1)
        | false => loop(charStack->String.concat(currentChar), index + 1)
        }
      }
    }
  }

  loop(str->String.slice(~start=0, ~end=1), 1)
}

let s1 = "abbaca"
let r1 = removeAllAdjDupesInString(s1)
Console.log2("r1: ", r1)

let s2 = "azxxzy"
let r2 = removeAllAdjDupesInString(s2)
Console.log2("r2: ", r2)
