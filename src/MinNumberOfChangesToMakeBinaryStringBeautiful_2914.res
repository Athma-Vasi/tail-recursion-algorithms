// T(n) = O(n)
// S(n) = O(1)

let minNumberOfChangesToMakeBinaryStringBeautiful = (binary: string) => {
  let rec loop = (count: int, index: int) => {
    switch index > String.length(binary) - 2 {
    | true => count
    | false => {
        let firstChar = binary->String.charAt(index)
        let secondChar = binary->String.charAt(index + 1)

        firstChar === secondChar ? loop(count, index + 2) : loop(count + 1, index + 2)
      }
    }
  }

  loop(0, 0)
}

let s1 = "1001"
let r1 = minNumberOfChangesToMakeBinaryStringBeautiful(s1)
Console.log2("r1: ", r1) // 2

let s2 = "10"
let r2 = minNumberOfChangesToMakeBinaryStringBeautiful(s2)
Console.log2("r2: ", r2) // 1

let s3 = "0000"
let r3 = minNumberOfChangesToMakeBinaryStringBeautiful(s3)
Console.log2("r3: ", r3) // 0
