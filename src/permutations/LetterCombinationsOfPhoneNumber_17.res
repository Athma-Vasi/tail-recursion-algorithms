let letterCombinationsOfPhoneNumber = (digits: string) => {
  let t9Table = {
    "0": "0",
    "1": "1",
    "2": "abc",
    "3": "def",
    "4": "ghi",
    "5": "jkl",
    "6": "mno",
    "7": "pqrs",
    "8": "tuv",
    "9": "wxyz",
  }

  let rec digitsLoop = (combinations: array<string>, tempChars: array<string>, digitIndex: int) => {
    switch digitIndex === String.length(digits) {
    | true => combinations
    | false => {
        let digit = digits->String.charAt(digitIndex)
        let digitNum = switch Int.fromString(digit) {
        | None => -1
        | Some(n) => n
        }

        switch digitNum < 0 {
        | true => combinations
        | false => {
            let t9Chars = String.split(
              switch t9Table->Object.get(Int.toString(digitNum)) {
              | None => ""
              | Some(str) => str
              },
              "",
            )

            let firstChar = switch t9Chars->Array.at(0) {
            | None => ""
            | Some(c) => c
            }

            let tempCharsLength = Array.length(tempChars)

            switch Array.length(t9Chars) === 0 || firstChar === "0" || firstChar === "1" {
            | true => digitsLoop(combinations, tempChars, digitIndex + 1)
            | false =>
              switch tempCharsLength === 0 {
              // first digit
              | true => digitsLoop(t9Chars, t9Chars, digitIndex + 1)
              | false => {
                  // subsequent digits
                  // tempChars is the previous combination
                  let rec tempCharsLoop = (newCombsOuter: array<string>, tempCharsIndex: int) => {
                    switch tempCharsIndex === tempCharsLength {
                    | true => newCombsOuter
                    | false => {
                        let existingComb = switch combinations->Array.at(tempCharsIndex) {
                        | None => ""
                        | Some(str) => str
                        }

                        let rec t9CharsLoop = (newCombsInner: array<string>, t9CharsIndex: int) => {
                          switch t9CharsIndex === Array.length(t9Chars) {
                          | true => newCombsInner
                          | false => {
                              let t9Char = switch t9Chars->Array.at(t9CharsIndex) {
                              | None => ""
                              | Some(c) => c
                              }

                              t9CharsLoop(
                                newCombsInner->Array.concat([existingComb ++ t9Char]),
                                t9CharsIndex + 1,
                              )
                            }
                          }
                        }

                        let newCombsInner = t9CharsLoop([], 0)
                        tempCharsLoop(
                          newCombsOuter->Array.concat(newCombsInner),
                          tempCharsIndex + 1,
                        )
                      }
                    }
                  }

                  let newCombinations = tempCharsLoop([], 0)
                  digitsLoop(newCombinations, t9Chars, digitIndex + 1)
                }
              }
            }
          }
        }
      }
    }
  }

  digitsLoop([], [], 0)
}

let d1 = "123"
let r1 = letterCombinationsOfPhoneNumber(d1)
Console.log2("r1: ", r1) // ["ad", "ae", "af", "bd", "be", "bf", "cd", "ce", "cf"]

let d2 = "23"
let r2 = letterCombinationsOfPhoneNumber(d2)
Console.log2("r2: ", r2) // ["ad", "ae", "af", "bd", "be", "bf", "cd", "ce", "cf"]

let d3 = "2"
let r3 = letterCombinationsOfPhoneNumber(d3)
Console.log2("r3: ", r3) // ["a", "b", "c"]

let d4 = "9"
let r4 = letterCombinationsOfPhoneNumber(d4)
Console.log2("r4: ", r4) // ["w", "x", "y", "z"]

let d5 = "0"
let r5 = letterCombinationsOfPhoneNumber(d5)
Console.log2("r5: ", r5) // []

let d6 = "1"
let r6 = letterCombinationsOfPhoneNumber(d6)
Console.log2("r6: ", r6) // []

let d7 = ""
let r7 = letterCombinationsOfPhoneNumber(d7)
Console.log2("r7: ", r7) // []
