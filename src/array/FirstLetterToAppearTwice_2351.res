// T(n) = O(n)
// S(n) = O(n)

let firstLetterToAppearTwice = (str: string) => {
  let rec travel = (letter: string, seen: Set.t<string>, index: int) => {
    switch index === String.length(str) || letter !== "" {
    | true => letter
    | false => {
        let char = str->String.charAt(index)
        let hasSeen = seen->Set.has(char)
        switch hasSeen {
        | true => travel(char, seen, index + 1)
        | false => {
            seen->Set.add(char)
            travel("", seen, index + 1)
          }
        }
      }
    }
  }

  travel("", Set.make(), 0)
}

let s1 = "abccbaacz"
let r1 = firstLetterToAppearTwice(s1)
Console.log2("r1: ", r1) // "c"

let s2 = "abcdd"
let r2 = firstLetterToAppearTwice(s2)
Console.log2("r2: ", r2) // "d"
