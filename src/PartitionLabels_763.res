let partitionLabels = (str: string): array<int> => {
  let length = String.length(str)
  let charIndexMap = String.split(str, "")->Array.reduceWithIndex(Map.make(), (
    mapAcc,
    char,
    index,
  ) => {
    mapAcc->Map.set(char, index)
    mapAcc
  })

  let rec loop = (
    accumulator: array<int>,
    ~anchorIndex: int,
    ~maxAnchorIndex: int,
    ~explorerIndex: int,
  ) => {
    let explorerChar = switch str->String.get(explorerIndex) {
    | None => ""
    | Some(char) => char
    }
    let maxExplorerIndex = switch charIndexMap->Map.get(explorerChar) {
    | None => -1
    | Some(num) => num
    }

    switch anchorIndex === length {
    | true => accumulator
    | false =>
      switch maxExplorerIndex === maxAnchorIndex {
      | true => {
          let newAnchorIndex = maxExplorerIndex + 1
          let newAcc = accumulator->Array.concat([newAnchorIndex - anchorIndex])
          let newAnchorChar = switch str->String.get(newAnchorIndex) {
          | None => ""
          | Some(char) => char
          }
          let newMaxAnchorIndex = switch charIndexMap->Map.get(newAnchorChar) {
          | None => -1
          | Some(num) => num
          }

          loop(
            newAcc,
            ~anchorIndex=newAnchorIndex,
            ~maxAnchorIndex=newMaxAnchorIndex,
            ~explorerIndex=newAnchorIndex + 1,
          )
        }
      | false =>
        switch maxExplorerIndex < maxAnchorIndex {
        | true => loop(accumulator, ~anchorIndex, ~maxAnchorIndex, ~explorerIndex=explorerIndex + 1)
        | false =>
          loop(
            accumulator,
            ~anchorIndex,
            ~maxAnchorIndex=maxExplorerIndex,
            ~explorerIndex=explorerIndex + 1,
          )
        }
      }
    }
  }

  let anchorChar = switch str->String.get(0) {
  | None => ""
  | Some(char) => char
  }
  let maxAnchorIndex = switch charIndexMap->Map.get(anchorChar) {
  | None => -1
  | Some(num) => num
  }

  loop([], ~anchorIndex=0, ~maxAnchorIndex, ~explorerIndex=1)
}

let s1 = "ababcbacadefegdehijhklij"
let r1 = partitionLabels(s1)
Console.log2("ababcbacadefegdehijhklij", r1)

let s2 = "eccbbbbdec"
let r2 = partitionLabels(s2)
Console.log2("eccbbbbdec", r2)
