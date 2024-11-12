// T(n) = O(n)
// S(n) = O(1)
// incorrect

let countSubstringsThatSatisfyKConstraint_I = (str: string, k: int) => {
  let rec slideWindow = (
    ~result: int,
    ~zeroesCount: int,
    ~onesCount: int,
    ~leftIndex: int,
    ~rightIndex: int,
  ) => {
    switch rightIndex === String.length(str) {
    | true => result
    | false =>
      switch leftIndex === rightIndex {
      | true =>
        // expand right border
        slideWindow(~result, ~zeroesCount, ~onesCount, ~leftIndex, ~rightIndex=rightIndex + 1)
      | false =>
        switch zeroesCount > k && onesCount > k {
        // shrink left border
        | true => {
            let leftChar = str->String.charAt(leftIndex)

            slideWindow(
              ~result=result + (rightIndex - leftIndex + 1),
              ~zeroesCount=leftChar === "0" ? zeroesCount - 1 : zeroesCount,
              ~onesCount=leftChar === "1" ? onesCount - 1 : onesCount,
              ~leftIndex=leftIndex + 1,
              ~rightIndex,
            )
          }
        | false => {
            let rightChar = str->String.charAt(rightIndex)

            slideWindow(
              ~result,
              ~zeroesCount=rightChar === "0" ? zeroesCount + 1 : zeroesCount,
              ~onesCount=rightChar === "1" ? onesCount + 1 : onesCount,
              ~leftIndex,
              ~rightIndex=rightIndex + 1,
            )
          }
        }
      }
    }
  }

  let firstChar = str->String.charAt(0)
  slideWindow(
    ~result=0,
    ~zeroesCount=firstChar === "0" ? 1 : 0,
    ~onesCount=firstChar === "1" ? 1 : 0,
    ~leftIndex=0,
    ~rightIndex=1,
  )
}

let s1 = "10101"
let k1 = 1
let r1 = countSubstringsThatSatisfyKConstraint_I(s1, k1)
Console.log2("r1: ", r1) // 12

// let s2 = "1010101"
// let k2 = 2
// let r2 = countSubstringsThatSatisfyKConstraint_I(s2, k2)
// Console.log2("r2: ", r2) // 25

// let s3 = "11111"
// let k3 = 1
// let r3 = countSubstringsThatSatisfyKConstraint_I(s3, k3)
// Console.log2("r3: ", r3) // 15
