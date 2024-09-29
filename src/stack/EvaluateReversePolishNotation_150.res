// T(n) = O(n)
// S(n) = O(n)

let evaluateReversePolishNotation = (tokens: array<string>) => {
  let operatorsSet = ["+", "-", "*", "/"]->Array.reduce(Set.make(), (acc, op) => {
    acc->Set.add(op)
    acc
  })

  let rec evaluate = (stack: array<int>, index: int) => {
    switch index === Array.length(tokens) {
    | true => stack
    | false => {
        let curr = switch tokens->Array.at(index) {
        | None => ""
        | Some(t) => t
        }

        switch operatorsSet->Set.has(curr) {
        | true => {
            let prev = switch stack->Array.at(-1) {
            | None => Int32.min_int
            | Some(t) => t
            }

            let prevPrev = switch stack->Array.at(-2) {
            | None => Int32.max_int
            | Some(t) => t
            }

            switch curr {
            | "+" =>
              evaluate(
                stack->Array.slice(~start=0, ~end=-2)->Array.concat([prevPrev + prev]),
                index + 1,
              )
            | "-" =>
              evaluate(
                stack->Array.slice(~start=0, ~end=-2)->Array.concat([prevPrev - prev]),
                index + 1,
              )
            | "*" =>
              evaluate(
                stack->Array.slice(~start=0, ~end=-2)->Array.concat([prevPrev * prev]),
                index + 1,
              )
            | _ =>
              evaluate(
                stack->Array.slice(~start=0, ~end=-2)->Array.concat([prevPrev / prev]),
                index + 1,
              )
            }
          }
        | false => {
            let currNum = switch Int.fromString(curr) {
            | None => Int32.max_int
            | Some(n) => n
            }

            evaluate(stack->Array.concat([currNum]), index + 1)
          }
        }
      }
    }
  }

  switch evaluate([], 0)->Array.at(0) {
  | None => Int32.min_int
  | Some(n) => n
  }
}

let t1 = ["2", "1", "+", "3", "*"]
let r1 = evaluateReversePolishNotation(t1)
Console.log2("r1: ", r1) // 9

let t2 = ["4", "13", "5", "/", "+"]
let r2 = evaluateReversePolishNotation(t2)
Console.log2("r2: ", r2) // 6

let t3 = ["10", "6", "9", "3", "+", "-11", "*", "/", "*", "17", "+", "5", "+"]
let r3 = evaluateReversePolishNotation(t3)
Console.log2("r3: ", r3) // 22
