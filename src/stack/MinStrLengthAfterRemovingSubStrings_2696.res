// T(n) = O(n)
// S(n) = O(n)

let minStrLengthAfterRemovingSubStrings = (str: string) => {
  let strLength = String.length(str)

  let rec loop = (stack: string, index: int) => {
    let stackLength = String.length(stack)
    let currentChar = str->String.charAt(index)

    switch index === strLength {
    | true => stackLength
    | false =>
      switch str->String.charAt(index) {
      | "B" =>
        switch stack->String.charAt(stackLength - 1) {
        | "A" => loop(stack->String.slice(~start=0, ~end=stackLength - 1), index + 1)
        | _ => loop(stack->String.concat(currentChar), index + 1)
        }
      | "D" =>
        switch stack->String.charAt(stackLength - 1) {
        | "C" => loop(stack->String.slice(~start=0, ~end=stackLength - 1), index + 1)
        | _ => loop(stack->String.concat(currentChar), index + 1)
        }
      | _ => loop(stack->String.concat(currentChar), index + 1)
      }
    }
  }

  loop("", 0)
}

let s1 = "ABFCACDB"
let r1 = minStrLengthAfterRemovingSubStrings(s1)
Console.log2("r1: ", r1)

let s2 = "ACBBD"
let r2 = minStrLengthAfterRemovingSubStrings(s2)
Console.log2("r2: ", r2)
