// T(n) = O(n)
// S(n) = O(n)

let removeOutermostParentheses = (parens: string) => {
  let parensLength = String.length(parens)

  let rec loop = (
    ~primitiveParens: string,
    ~charStack: string,
    ~openCount: int,
    ~closeCount: int,
    ~index: int,
  ) => {
    let charStackLength = String.length(charStack)

    switch index === parensLength {
    | true => primitiveParens
    | false =>
      switch openCount === 1 && closeCount === 1 {
      | true => loop(~primitiveParens, ~charStack, ~openCount=0, ~closeCount=0, ~index=index + 1)
      | false =>
        switch openCount === closeCount && openCount > 1 && closeCount > 1 {
        | true =>
          loop(
            ~primitiveParens=primitiveParens->String.concat(
              charStack->String.slice(~start=1, ~end=charStackLength - 1),
            ),
            ~charStack="",
            ~openCount=0,
            ~closeCount=0,
            ~index=index + 1,
          )
        | false => {
            let currentParens = switch parens->String.get(index) {
            | None => ""
            | Some(char) => char
            }

            switch currentParens {
            | "(" =>
              loop(
                ~primitiveParens,
                ~charStack,
                ~openCount=openCount + 1,
                ~closeCount,
                ~index=index + 1,
              )
            // ")"
            | _ =>
              loop(
                ~primitiveParens,
                ~charStack,
                ~openCount,
                ~closeCount=closeCount + 1,
                ~index=index + 1,
              )
            }
          }
        }
      }
    }
  }

  loop(~primitiveParens="", ~charStack="", ~openCount=0, ~closeCount=0, ~index=0)
}

let s1 = "(()())(())"
let r1 = removeOutermostParentheses(s1)
Console.log2("r1 ==  ()()(): ", r1)

let s2 = "(()())(())(()(()))"
let r2 = removeOutermostParentheses(s2)
Console.log2("r2 ==  ()()()()(()): ", r2)

let s3 = "()()"
let r3 = removeOutermostParentheses(s3)
Console.log2("r3 ==  : ", r3)
