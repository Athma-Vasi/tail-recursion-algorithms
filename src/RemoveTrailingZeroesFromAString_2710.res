// T(n) = O(n)
// S(n) = O(1)

let removeTrailingZeroesFromAString = (numStr: string) => {
  let rec remove = (stop: bool, index: int) => {
    switch stop {
    | true => numStr->String.slice(~start=0, ~end=index + 1)
    | false =>
      switch numStr->String.charAt(index) === "0" {
      | true => remove(stop, index - 1)
      | false => remove(true, index)
      }
    }
  }

  remove(false, String.length(numStr) - 1)
}

let n1 = "51230100"
let r1 = removeTrailingZeroesFromAString(n1)
Console.log2("r1: ", r1) // 512301

let n2 = "123"
let r2 = removeTrailingZeroesFromAString(n2)
Console.log2("r2: ", r2) // 123
