// T(n) = O(n)
// S(n) = O(1)

let longerContiguousSegmentsOfOnes = (str: string) => {
  let strLength = String.length(str)

  let rec loop = (
    ~index: int,
    ~longestOnes: int,
    ~longestZeroes: int,
    ~onesCount: int,
    ~stack: string,
    ~zeroesCount: int,
  ) => {
    switch index === strLength {
    | true => longestOnes > longestZeroes
    | false => {
        let currentBinary = str->String.charAt(index)
        let previousBinary = str->String.charAt(strLength - 1)

        switch currentBinary === previousBinary {
        | true => {
            let newOnesCount = currentBinary === "1" ? onesCount + 1 : onesCount
            let newZeroesCount = currentBinary === "0" ? zeroesCount + 1 : zeroesCount

            loop(
              ~index=index + 1,
              ~longestOnes=longestOnes > newOnesCount ? longestOnes : newOnesCount,
              ~longestZeroes=longestZeroes > newZeroesCount ? longestZeroes : newZeroesCount,
              ~onesCount=newOnesCount,
              ~stack=stack->String.concat(currentBinary),
              ~zeroesCount=newZeroesCount,
            )
          }
        | false =>
          loop(
            ~index=index + 1,
            ~longestOnes,
            ~longestZeroes,
            ~onesCount=currentBinary === "1" ? 1 : onesCount,
            ~stack=stack->String.concat(currentBinary),
            ~zeroesCount=currentBinary === "0" ? 1 : zeroesCount,
          )
        }
      }
    }
  }

  loop(
    ~index=0,
    ~longestOnes=Int32.min_int,
    ~longestZeroes=Int32.min_int,
    ~onesCount=0,
    ~stack="",
    ~zeroesCount=0,
  )
}

let s1 = "1101"
let r1 = longerContiguousSegmentsOfOnes(s1)
Console.log2("r1: ", r1)

let s2 = "111000"
let r2 = longerContiguousSegmentsOfOnes(s2)
Console.log2("r2: ", r2)

let s3 = "110100010"
let r3 = longerContiguousSegmentsOfOnes(s3)
Console.log2("r3: ", r3)
