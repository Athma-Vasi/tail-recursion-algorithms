// T(n) = O(n)
// S(n) = O(n)

let backspaceStringCompare = (string1: string, string2: string) => {
  let rec loop = (charStack: string, str: string, index: int) => {
    let strLength = String.length(str)

    switch index === strLength {
    | true => charStack
    | false => {
        let currentChar = switch str->String.get(index) {
        | None => ""
        | Some(char) => char
        }

        switch currentChar {
        | "#" =>
          loop(charStack->String.slice(~start=0, ~end=String.length(charStack) - 1), str, index + 1)
        | char => loop(charStack->String.concat(char), str, index + 1)
        }
      }
    }
  }

  loop("", string1, 0) === loop("", string2, 0)
}

let s1 = "ab#c"
let s11 = "ad#c"
let r1 = backspaceStringCompare(s1, s11)
Console.log2("r1: ", r1)

let s2 = "ab##"
let s22 = "c#d#"
let r2 = backspaceStringCompare(s2, s22)
Console.log2("r2: ", r2)

let s3 = "a##c"
let s33 = "#a#c"
let r3 = backspaceStringCompare(s3, s33)
Console.log2("r3: ", r3)

let s4 = "a#c"
let s44 = "b"
let r4 = backspaceStringCompare(s4, s44)
Console.log2("r4: ", r4)
