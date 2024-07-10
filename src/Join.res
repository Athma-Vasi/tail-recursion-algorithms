let join = (strings: array<string>, ~spacer=" "): string => {
  let rec helper = (accumulator: string, rest: array<string>, index: int): string => {
    let length = Array.length(rest)

    switch length {
    | 0 => accumulator
    | _ => {
        let head = switch rest->Array.get(0) {
        | None => ""
        | Some(str) => str
        }

        let sliced = rest->Array.slice(~start=1, ~end=length)

        index === 0 || index === length
          ? helper(accumulator ++ head, sliced, index + 1)
          : helper(accumulator ++ spacer ++ head, sliced, index + 1)
      }
    }
  }

  helper("", strings, 0)
}

let strings = ["Hello", "Athma", "!", "How", "are", "you", "?"]
let result = join(strings)
Console.log2("join", result)
