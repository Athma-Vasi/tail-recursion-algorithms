// T(n) = O(n)
// S(n) = O(n)

let removingStarsFromAString = (str: string) => {
  let rec removeStars = (stack: string, index: int) => {
    switch index === String.length(str) {
    | true => stack
    | false => {
        let curr = str->String.charAt(index)

        switch curr {
        | "*" =>
          removeStars(stack->String.slice(~start=0, ~end=String.length(stack) - 1), index + 1)
        | _ => removeStars(stack->String.concat(curr), index + 1)
        }
      }
    }
  }

  removeStars(String.make(), 0)
}

let s1 = "leet**cod*e"
let r1 = removingStarsFromAString(s1)
Console.log2("r1: ", r1) // "lecoe"

let s2 = "erase*****"
let r2 = removingStarsFromAString(s2)
Console.log2("r2: ", r2) // ""
