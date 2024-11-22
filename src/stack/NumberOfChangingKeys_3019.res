// T(n) = O(n)
// S(n) = O(1)

let numberOfChangingKeys = (keys: string) => {
  let lowercased = String.toLowerCase(keys)

  let rec loop = (count: int, stack: string, index: int) => {
    switch index === String.length(keys) {
    | true => count
    | false => {
        let currKey = lowercased->String.charAt(index)
        let prevKey = stack->String.charAt(String.length(stack) - 1)

        loop(
          currKey === prevKey || String.length(prevKey) === 0 ? count : count + 1,
          stack->String.concat(currKey),
          index + 1,
        )
      }
    }
  }

  loop(0, String.make(), 0)
}

let s1 = "aAbBcC"
let r1 = numberOfChangingKeys(s1)
Console.log2("r1: ", r1)

let s2 = "AaAaAaaA"
let r2 = numberOfChangingKeys(s2)
Console.log2("r2: ", r2)
