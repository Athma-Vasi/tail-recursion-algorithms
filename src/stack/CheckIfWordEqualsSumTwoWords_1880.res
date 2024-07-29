let checkIfWordEqualsSumTwoWords = (
  firstWord: string,
  secondWord: string,
  targetWord: string,
): bool => {
  let numericalValueTable = {
    "a": "0",
    "b": "1",
    "c": "2",
    "d": "3",
    "e": "4",
    "f": "5",
    "g": "6",
    "h": "7",
    "i": "8",
    "j": "9",
  }

  let rec calculateNumericalValue = (charStack: string, index: int, word: string) => {
    switch index === String.length(word) {
    | true => charStack
    | false => {
        let char = word->String.charAt(index)
        let numericalValue = switch numericalValueTable->Object.get(char) {
        | None => ""
        | Some(value) => value
        }

        calculateNumericalValue(charStack->String.concat(numericalValue), index + 1, word)
      }
    }
  }

  let numericalValue1 = switch calculateNumericalValue("", 0, firstWord)->Int.fromString {
  | None => Int32.min_int
  | Some(num) => num
  }
  let numericalValue2 = switch calculateNumericalValue("", 0, secondWord)->Int.fromString {
  | None => Int32.max_int
  | Some(num) => num
  }
  let sum = numericalValue1 + numericalValue2

  let numericalValueTarget = switch calculateNumericalValue("", 0, targetWord)->Int.fromString {
  | None => Int32.min_int
  | Some(num) => num
  }

  sum === numericalValueTarget
}

let w1 = "acb"
let w11 = "cba"
let w111 = "cdb"
let r1 = checkIfWordEqualsSumTwoWords(w1, w11, w111)
Console.log2("r1: ", r1)

let w2 = "aaa"
let w22 = "a"
let w222 = "aab"
let r2 = checkIfWordEqualsSumTwoWords(w2, w22, w222)
Console.log2("r2: ", r2)

let w3 = "aaa"
let w33 = "a"
let w333 = "aaaa"
let r3 = checkIfWordEqualsSumTwoWords(w3, w33, w333)
Console.log2("r3: ", r3)
