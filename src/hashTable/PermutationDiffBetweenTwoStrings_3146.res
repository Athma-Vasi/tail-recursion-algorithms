// T(n) = O(n)
// S(n) = O(n)

let permutationDiffBetweenTwoStrings = (s1: string, s2: string) => {
  let rec s1Loop = (indexTable: Map.t<string, int>, index1: int) => {
    switch index1 === String.length(s1) {
    | true => indexTable
    | false => {
        let char = s1->String.charAt(index1)
        indexTable->Map.set(char, index1)

        s1Loop(indexTable, index1 + 1)
      }
    }
  }

  let indexTable = s1Loop(Map.make(), 0)

  let rec s2Loop = (permutationDiff: int, index2: int) => {
    switch index2 === String.length(s2) {
    | true => permutationDiff
    | false => {
        let char = s2->String.charAt(index2)
        let charIdx = switch indexTable->Map.get(char) {
        | None => -1
        | Some(i) => i
        }
        let newDiff = charIdx - index2
        let absDiff = newDiff < 0 ? newDiff * -1 : newDiff

        s2Loop(permutationDiff + absDiff, index2 + 1)
      }
    }
  }

  s2Loop(0, 0)
}

let s1 = "abc"
let s11 = "bac"
let r1 = permutationDiffBetweenTwoStrings(s1, s11)
Console.log2("r1: ", r1) // 2

let s2 = "abcde"
let s22 = "edbac"
let r2 = permutationDiffBetweenTwoStrings(s2, s22)
Console.log2("r2: ", r2) // 12
