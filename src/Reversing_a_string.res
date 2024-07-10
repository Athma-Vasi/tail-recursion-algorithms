let reverseString = (str: string): string => {
  let rec helper = (accumulator: string, tail: string): string => {
    let length = String.length(tail)

    switch length {
    | 0 => accumulator
    | _ => {
        let head = switch tail->String.get(0) {
        | None => ""
        | Some(character) => character
        }
        let rest = tail->String.slice(~start=1, ~end=length)

        helper(head ++ accumulator, rest)
      }
    }
  }

  helper("", str)
}

let string = "Hello, World!"
let result = reverseString(string)
Console.log2("reversed string", result)
