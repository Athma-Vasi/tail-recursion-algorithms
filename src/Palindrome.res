// T(n) = O(n)
// S(n) = O(n)

let palindrome = (str: string): bool => {
  let rec helper = (middle: string): bool => {
    let length = String.length(middle)

    switch length {
    | 0 => true
    | _ => {
        let head = switch middle->String.get(0) {
        | None => ""
        | Some(character) => character
        }

        let tail = switch middle->String.get(length - 1) {
        | None => ""
        | Some(character) => character
        }

        let sliced = middle->String.slice(~start=1, ~end=length - 1)

        head === tail && helper(sliced)
      }
    }
  }

  helper(str)
}

let string = "racecar"
let result = palindrome(string)
Console.log2("racecar is palindrome", result)
let string2 = "amanaplanacanalpanama"
let result2 = palindrome(string2)
Console.log2("amanaplanacanalpanama is palindrome", result2)
let string3 = "tacocat"
let result3 = palindrome(string3)
Console.log2("tacocat is palindrome", result3)
