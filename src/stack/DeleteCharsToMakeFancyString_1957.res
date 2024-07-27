// T(n) = O(n)
// S(n) = O(n)

let deleteCharsToMakeFancyString = (str: string) => {
  let length = String.length(str)

  let rec loop = (charStack: string, index: int, counter: int) => {
    let charStackLength = String.length(charStack)

    switch index === length {
    | true => charStack
    | false => {
        let previousChar = charStack->String.charAt(charStackLength - 1)
        let currentChar = str->String.charAt(index)

        switch currentChar === previousChar {
        | true =>
          counter === 2
            ? loop(charStack, index + 1, counter)
            : loop(charStack->String.concat(currentChar), index + 1, 2)
        | false => loop(charStack->String.concat(currentChar), index + 1, 0)
        }
      }
    }
  }

  loop("", 0, 0)
}

let s1 = "leeetcode"
let r1 = deleteCharsToMakeFancyString(s1)
Console.log2("r1: ", r1)

let s2 = "aaabaaaa"
let r2 = deleteCharsToMakeFancyString(s2)
Console.log2("r2: ", r2)
