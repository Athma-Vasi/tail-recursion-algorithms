let truncateSentence = (str: string, k: int) => {
  let rec loop = (charStack: string, index: int, spacesCount: int) => {
    switch index === String.length(str) || spacesCount === k {
    | true => charStack
    | false => {
        let char = str->String.charAt(index)

        loop(
          charStack->String.concat(char),
          index + 1,
          char === " " ? spacesCount + 1 : spacesCount,
        )
      }
    }
  }

  loop("", 0, 0)
}

let s1 = "Hello how are you Contestant"
let k1 = 4
let r1 = truncateSentence(s1, k1) // "Hello how are you"
Console.log2("r1: ", r1)

let s2 = "What is the solution to this problem"
let k2 = 4
let r2 = truncateSentence(s2, k2) // "What is the solution"
Console.log2("r2: ", r2)

let s3 = "chopper is not a tanuki"
let k3 = 5
let r3 = truncateSentence(s3, k3) // "chopper is not a tanuki"
Console.log2("r3: ", r3)
