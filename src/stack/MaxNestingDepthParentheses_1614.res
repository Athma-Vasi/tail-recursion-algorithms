// T(n) = O(n)
// S(n) = O(n)

let maxNestingDepthParentheses = (str: string) => {
  let strLength = String.length(str)

  let rec loop = (max: int, parensStack: string, index: int) => {
    let parensStackLength = String.length(parensStack)

    switch index === strLength {
    | true => max + 1
    | false =>
      switch str->String.charAt(index) {
      | "(" =>
        loop(
          max > parensStackLength ? max : parensStackLength,
          parensStack->String.concat("("),
          index + 1,
        )
      | ")" => loop(max, parensStack->String.slice(~start=0, ~end=parensStackLength - 1), index + 1)
      | _ => loop(max, parensStack, index + 1)
      }
    }
  }

  loop(Int32.min_int, "", 0)
}

let s1 = "(1+(2*3)+((8)/4))+1"
let r1 = maxNestingDepthParentheses(s1)
Console.log2("r1: ", r1)

let s2 = "(1)+((2))+(((3)))"
let r2 = maxNestingDepthParentheses(s2)
Console.log2("r2: ", r2)

let s3 = "()(())((()()))"
let r3 = maxNestingDepthParentheses(s3)
Console.log2("r3: ", r3)
