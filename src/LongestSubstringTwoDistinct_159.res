let longestSubstring159 = (str: string): int => {
  let length = String.length(str)

  let rec loop = (~charCountMap, ~longestSubStr: int, ~lowIndex: int, ~highIndex: int): int => {
    switch highIndex === length - 1 {
    | true => longestSubStr
    | false =>
      switch Map.size(charCountMap) > 2 {
      | true => {
          let currentLowChar = switch str->String.get(lowIndex) {
          | None => ""
          | Some(char) => char
          }
          let prevLowCount = switch charCountMap->Map.get(currentLowChar) {
          | None => 0
          | Some(num) => num
          }

          let newLowCount = prevLowCount - 1
          switch newLowCount === 0 {
          | true => {
              let _ = charCountMap->Map.delete(currentLowChar)
            }
          | false => charCountMap->Map.set(currentLowChar, newLowCount)
          }

          loop(~charCountMap, ~longestSubStr=longestSubStr - 1, ~lowIndex=lowIndex + 1, ~highIndex)
        }
      | false => {
          let newHighIndex = highIndex + 1
          let newHighChar = switch str->String.get(newHighIndex) {
          | None => ""
          | Some(char) => char
          }
          let existingHighCount = switch charCountMap->Map.get(newHighChar) {
          | None => 0
          | Some(num) => num
          }

          charCountMap->Map.set(newHighChar, existingHighCount + 1)

          loop(~charCountMap, ~longestSubStr=longestSubStr + 1, ~lowIndex, ~highIndex=newHighIndex)
        }
      }
    }
  }

  switch length < 2 {
  | true => length
  | false => {
      let charCountMap = Map.make()
      let firstChar = switch str->String.get(0) {
      | None => ""
      | Some(char) => char
      }
      let secondChar = switch str->String.get(1) {
      | None => ""
      | Some(char) => char
      }

      switch firstChar === secondChar {
      | true => charCountMap->Map.set(firstChar, 2)
      | false => {
          charCountMap->Map.set(firstChar, 1)
          charCountMap->Map.set(secondChar, 1)
        }
      }

      loop(~charCountMap, ~longestSubStr=2, ~lowIndex=0, ~highIndex=1)
    }
  }
}

let s1 = "eceba"
let r1 = longestSubstring159(s1)
Console.log2("eceba", r1)

let s2 = "ccaabbb"
let r2 = longestSubstring159(s2)
Console.log2("ccaabbb", r2)
