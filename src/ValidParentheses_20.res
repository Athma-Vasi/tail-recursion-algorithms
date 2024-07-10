// T(n) = O(n)
// S(n) = O(n)

let validParentheses = (str: string): bool => {
  let pairTable = {
    ")": "(",
    "}": "{",
    "]": "[",
  }

  let openingParens = ["(", "{", "["]->Array.reduceRight(Set.make(), (setAcc, currStr) => {
    setAcc->Set.add(currStr)
    setAcc
  })

  let rec loop = (stack: array<string>, isValid: bool, newStr: string) => {
    let strLength = String.length(newStr)
    let firstChar = newStr->String.slice(~start=0, ~end=1)
    let restChars = newStr->String.slice(~start=1, ~end=strLength)

    switch openingParens->Set.has(firstChar) {
    | true => loop(stack->Array.concat([firstChar]), isValid, restChars)
    | false => {
        let correctOpeningParens = switch pairTable->Object.get(firstChar) {
        | None => ""
        | Some(char) => char
        }
        let stackLength = Array.length(stack)
        let popped = switch stack->Array.get(stackLength - 1) {
        | None => ""
        | Some(char) => char
        }
        let sliced = stack->Array.slice(~start=0, ~end=stackLength - 1)

        switch correctOpeningParens === "" || popped === "" {
        | true => isValid
        | false =>
          switch correctOpeningParens === popped {
          | true => loop(sliced, isValid, restChars)
          | false => false
          }
        }
      }
    }
  }

  switch String.length(str) > 1 {
  | true => loop([], true, str)
  | false => false
  }
}

let s1 = "()"
let r1 = validParentheses(s1)
Console.log2(s1, r1)

let s2 = "()[]{}"
let r2 = validParentheses(s2)
Console.log2(s2, r2)

let s3 = "(]"
let r3 = validParentheses(s3)
Console.log2(s3, r3)

let s4 = "([{}])"
let r4 = validParentheses(s4)
Console.log2(s4, r4)

let s5 = "((({})))"
let r5 = validParentheses(s5)
Console.log2(s5, r5)

let s6 = "({[()]})"
let r6 = validParentheses(s6)
Console.log2(s6, r6)

let s4 = "([{}])"
let r4 = validParentheses(s4)
Console.log2(s4, r4)

let s5 = "((({})))"
let r5 = validParentheses(s5)
Console.log2(s5, r5)

let s6 = ")"
let r6 = validParentheses(s6)
Console.log2(s6, r6)
