// T(n) = O(n + log(k))
// S(n) = O(n)

let findKthCharInStringGame = (k: int) => {
  let rec outerLoop = (word: string) => {
    switch String.length(word) >= k {
    | true => word
    | false => {
        let rec innerLoop = (genStr: string, index: int) => {
          switch index === String.length(word) {
          | true => genStr
          | false => {
              let char = word->String.charAt(index)
              let charCode = char->String.charCodeAt(0)
              let nextChar = String.fromCharCode(Int.fromFloat(charCode +. 1.0))

              innerLoop(genStr->String.concat(nextChar), index + 1)
            }
          }
        }

        let genStr = innerLoop(word, 0)
        outerLoop(genStr)
      }
    }
  }

  let result = outerLoop("a")
  result->String.charAt(k - 1)
}

let k = 5
let r1 = findKthCharInStringGame(k)
Console.log2("r1: ", r1) // "b"

let k = 10
let r2 = findKthCharInStringGame(k)
Console.log2("r2: ", r2) // "c"
