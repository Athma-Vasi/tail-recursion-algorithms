// T(n) = O(n)
// S(n) = O(n)

let scoreOfAString = (str: string) => {
  let rec loop = (score: int, stack: array<int>, index: int) => {
    switch index === String.length(str) {
    | true => score
    | false => {
        let prevCharCode = switch stack->Array.at(-1) {
        | None => -1
        | Some(n) => n
        }
        let currCharCode = switch String.codePointAt(str, index) {
        | None => -1
        | Some(n) => n
        }
        let diff = prevCharCode - currCharCode
        let absDiff = diff < 0 ? diff * -1 : diff

        loop(
          prevCharCode < 0 ? score : score + absDiff,
          stack->Array.concat([currCharCode]),
          index + 1,
        )
      }
    }
  }

  loop(0, [], 0)
}

let s1 = "hello"
let r1 = scoreOfAString(s1)
Console.log2("r1: ", r1) // 13

let s2 = "zaz"
let r2 = scoreOfAString(s2)
Console.log2("r2: ", r2) // 50
