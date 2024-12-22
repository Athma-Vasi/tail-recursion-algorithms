// INCORRECT - infinite loop

let letterTilePossibilities = (tiles: string) => {
  let checkPossibilities = (subsets: Set.t<string>) => {
    let rec backtrack = (remainingTiles: string, currentCombination: string, charIndex: int) => {
      subsets->Set.add(currentCombination)

      switch charIndex === String.length(remainingTiles) {
      | true => ()
      | false =>
        backtrack(
          remainingTiles->String.substring(~start=0, ~end=charIndex) ++
            remainingTiles->String.substringToEnd(~start=charIndex + 1),
          currentCombination ++ remainingTiles->String.charAt(charIndex),
          charIndex + 1,
        )
      }
    }

    backtrack(tiles, String.make(), 0)
  }

  checkPossibilities(Set.make())
}

let t1 = "AAB"
let r1 = letterTilePossibilities(t1)
Console.log2("r1: ", r1) // 8

// let t2 = "AAABBC"
// let r2 = letterTilePossibilities(t2)
// Console.log2("r2: ", r2) // 188

// let t3 = "V"
// let r3 = letterTilePossibilities(t3)
// Console.log2("r3: ", r3) // 1
