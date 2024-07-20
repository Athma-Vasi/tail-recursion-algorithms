// T(n) = O(m * n) where m <= n
// S(n) = O(m)
// incorrect, only finds first valid anagram

let anagrams = (str: string, subString: string) => {
  let stringLength = String.length(str)

  let makeSubStringTable = (subString: string): Map.t<string, int> =>
    String.split(subString, "")->Array.reduce(Map.make(), (acc, char) => {
      let existingCount = switch acc->Map.get(char) {
      | None => 0
      | Some(count) => count
      }
      acc->Map.set(char, existingCount + 1)
      acc
    })

  let rec updateTable = (subStringTable: Map.t<string, int>, counter: int, index: int) => {
    switch counter > Map.size(subStringTable) {
    | true => subStringTable
    | false => {
        let char = switch str->String.get(counter + index) {
        | None => ""
        | Some(c) => c
        }

        Console.log("--updateTable--")
        Console.log2("char", char)

        switch subStringTable->Map.has(char) {
        | true => {
            let existingCount = switch subStringTable->Map.get(char) {
            | None => 0
            | Some(count) => count
            }
            subStringTable->Map.set(char, existingCount - 1)
          }
        | false => ()
        }

        Console.log2("subStringTable", subStringTable)

        updateTable(subStringTable, counter + 1, index)
      }
    }
  }

  let rec loop = (
    startIndices: array<int>,
    subStringTable: Map.t<string, int>,
    leftIndex: int,
    rightIndex: int,
  ): array<int> => {
    let counts =
      subStringTable
      ->Map.entries
      ->Array.fromIterator
      ->Array.reduce(0, (acc, (_char, count)) => acc + count)

    switch rightIndex === stringLength {
    // if at end of string
    | true =>
      switch counts === 0 {
      // and valid anagram exists at max right window
      | true => startIndices->Array.concat([leftIndex])
      | false => startIndices
      }
    // if not at end of str
    | false =>
      switch counts === 0 {
      // and valid anagram exists
      | true => {
          let initialSubStringTable = makeSubStringTable(subString)
          let updatedSubStringTable = updateTable(initialSubStringTable, 1, leftIndex)

          Console.log("--loop--")
          Console.log2("leftIndex", leftIndex)
          Console.log2("rightIndex", rightIndex)
          Console.log2("startIndices", startIndices)
          Console.log2("updatedSubStringTable", updatedSubStringTable)

          let leftChar = switch str->String.get(leftIndex) {
          | None => ""
          | Some(char) => char
          }
          switch updatedSubStringTable->Map.has(leftChar) {
          | true => {
              let existingCount = switch updatedSubStringTable->Map.get(leftChar) {
              | None => 0
              | Some(count) => count
              }
              // move left char out of window and increment count
              updatedSubStringTable->Map.set(leftChar, existingCount + 1)
            }
          | false => ()
          }

          let rightChar = switch str->String.get(rightIndex + 1) {
          | None => ""
          | Some(char) => char
          }
          switch updatedSubStringTable->Map.has(rightChar) {
          | true => {
              let existingCount = switch updatedSubStringTable->Map.get(rightChar) {
              | None => 0
              | Some(count) => count
              }
              // move right char into window and decrement count
              updatedSubStringTable->Map.set(rightChar, existingCount - 1)
            }
          | false => ()
          }

          Console.log2("leftChar", leftChar)
          Console.log2("rightChar", rightChar)
          Console.log2("updatedSubStringTable", updatedSubStringTable)

          // slide window right
          loop(
            startIndices->Array.concat([leftIndex]),
            updatedSubStringTable,
            leftIndex + 1,
            rightIndex + 1,
          )
        }
      // and valid anagram does not exist
      | false => {
          let leftChar = switch str->String.get(leftIndex) {
          | None => ""
          | Some(char) => char
          }
          switch subStringTable->Map.has(leftChar) {
          | true => {
              let existingCount = switch subStringTable->Map.get(leftChar) {
              | None => 0
              | Some(count) => count
              }
              // move left char out of window and increment count
              subStringTable->Map.set(leftChar, existingCount + 1)
            }
          | false => ()
          }

          let rightChar = switch str->String.get(rightIndex + 1) {
          | None => ""
          | Some(char) => char
          }
          switch subStringTable->Map.has(rightChar) {
          | true => {
              let existingCount = switch subStringTable->Map.get(rightChar) {
              | None => 0
              | Some(count) => count
              }
              // move right char into window and decrement count
              subStringTable->Map.set(rightChar, existingCount - 1)
            }
          | false => ()
          }

          Console.log("--valid anagram does not exist--")
          Console.log2("leftChar", leftChar)
          Console.log2("rightChar", rightChar)
          Console.log2("subStringTable", subStringTable)

          // slide window right
          loop(startIndices, subStringTable, leftIndex + 1, rightIndex + 1)
        }
      }
    }
  }

  let initialSubStringTable = makeSubStringTable(subString)
  let updatedSubStringTable = updateTable(initialSubStringTable, 1, -1)

  Console.log("--before loop--")
  Console.log2("updatedSubStringTable", updatedSubStringTable)

  loop([], updatedSubStringTable, 0, String.length(subString) - 1)
}

let s1 = "cbaebabacd"
let p1 = "abc"
let r1 = anagrams(s1, p1)
Console.log2("cbaebabacd   abc", r1)
