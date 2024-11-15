// T(n) = O(n)
// S(n) = O(n)

let clearDigits = (str: string) => {
  let rec loop = (stack: string, index: int) => {
    switch index === String.length(str) {
    | true => stack
    | false => {
        let char = str->String.charAt(index)
        let charNum = switch Int.fromString(char) {
        | None => -1
        | Some(n) => n
        }

        charNum < 0
          ? loop(stack->String.concat(char), index + 1)
          : loop(stack->String.slice(~start=0, ~end=String.length(stack) - 1), index + 1)
      }
    }
  }

  loop(String.make(), 0)
}

let s1 = "abc"
let r1 = clearDigits(s1)
Console.log2("r1: ", r1) // "abc"

let s2 = "cb34"
let r2 = clearDigits(s2)
Console.log2("r2: ", r2) // "" empty string
