// T(n) = O(n)
// S(n) = O(1)

let lengthLastWord = (words: string) => {
  let rec loop = (~charCount: int, ~numOfSpaces: int, ~prevChar: string, ~index: int) => {
    switch numOfSpaces === 2 {
    | true => charCount - 1
    | false => {
        let char = switch words->String.get(index) {
        | None => " "
        | Some(ch) => ch
        }

        switch char === " " {
        | true => loop(~charCount, ~numOfSpaces, ~prevChar=" ", ~index=index - 1)
        | false =>
          switch prevChar === " " {
          | true =>
            loop(
              ~charCount=charCount + 1,
              ~numOfSpaces=numOfSpaces + 1,
              ~prevChar=char,
              ~index=index - 1,
            )
          | false => loop(~charCount=charCount + 1, ~numOfSpaces, ~prevChar=char, ~index=index - 1)
          }
        }
      }
    }
  }

  loop(~charCount=0, ~numOfSpaces=0, ~prevChar=" ", ~index=String.length(words) - 1)
}

let s1 = "Hello World "
let r1 = lengthLastWord(s1)
Console.log2("r1: ", r1)

let s2 = "   fly me   to   the moon  "
let r2 = lengthLastWord(s2)
Console.log2("r2: ", r2)

let s3 = "luffy is still joyboy"
let r3 = lengthLastWord(s3)
Console.log2("r3: ", r3)
