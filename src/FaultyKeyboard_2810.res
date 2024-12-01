// T(n) = O(n)
// S(n) = O(n)

let faultyKeyboard = (s: string) => {
  let length = String.length(s)

  let reverseString = (original: string) => {
    let rec reverse = (reversed: string, index: int) => {
      switch index < 0 {
      | true => reversed
      | false => reverse(reversed->String.concat(original->String.charAt(index)), index - 1)
      }
    }

    reverse(String.make(), length - 1)
  }

  let rec makeFinalString = (finalString: string, index: int) => {
    switch index === length {
    | true => finalString
    | false => {
        let char = s->String.charAt(index)

        switch char {
        | "i" => makeFinalString(reverseString(finalString), index + 1)
        | _ => makeFinalString(finalString->String.concat(char), index + 1)
        }
      }
    }
  }

  makeFinalString(String.make(), 0)
}

let s1 = "string"
let r1 = faultyKeyboard(s1)
Console.log2("r1: ", r1) // "rtsng"

let s2 = "poiinter"
let r2 = faultyKeyboard(s2)
Console.log2("r2: ", r2) // "ponter"
