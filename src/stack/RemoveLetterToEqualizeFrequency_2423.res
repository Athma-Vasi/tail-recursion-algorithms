// T(n) = O(n)
// S(n) = O(1)

let removeLetterToEqualizeFrequency = (word: string) => {
  let rec loop = (stack: string, removed: int, index: int) => {
    switch index === String.length(word) {
    | true => removed === 1
    | false => {
        let currChar = word->String.charAt(index)
        let prevChar = stack->String.charAt(String.length(stack) - 1)

        switch currChar === prevChar {
        | true => loop(stack, removed + 1, index + 1)
        | false => loop(stack->String.concat(currChar), removed, index + 1)
        }
      }
    }
  }

  let firstChar = word->String.charAt(0)
  loop(firstChar, 0, 1)
}

let w1 = "abcc"
let r1 = removeLetterToEqualizeFrequency(w1)
Console.log2("r1: ", r1) // true

let w2 = "aazz"
let r2 = removeLetterToEqualizeFrequency(w2)
Console.log2("r2: ", r2) // false
