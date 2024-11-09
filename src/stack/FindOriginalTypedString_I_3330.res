// T(n) = O(n)
// S(n) = O(n)

let findOriginalTypedString = (word: string) => {
  let rec loop = (permutations: int, stack: string, index: int) => {
    let prevChar = stack->String.charAt(String.length(stack) - 1)
    let currChar = word->String.charAt(index)

    switch index === String.length(word) {
    | true => permutations
    | false =>
      prevChar === currChar
        ? loop(permutations + 1, stack->String.concat(currChar), index + 1)
        : loop(permutations, stack->String.concat(currChar), index + 1)
    }
  }

  loop(1, String.make(), 0)
}

let s1 = "abbcccc"
let r1 = findOriginalTypedString(s1)
Console.log2("r1: ", r1) // 5

let s2 = "abcd"
let r2 = findOriginalTypedString(s2)
Console.log2("r2: ", r2) // 1
