// T(n) = O(n)
// S(n) = O(n)

type truck = G | P | M

let minAmountOfTimeToCollectGarbage = (garbages: array<string>, travelTimes: array<int>) => {
  let rec makeMaxIndexTable = (table: Map.t<string, int>, index: int) => {
    switch index === Array.length(garbages) {
    | true => table
    | false => {
        let garbage = switch garbages->Array.at(index) {
        | None => String.make()
        | Some(s) => s
        }
        let set = Set.fromArray(garbage->String.split(""))
        set->Set.has("G") ? table->Map.set("G", index) : ()
        set->Set.has("P") ? table->Map.set("P", index) : ()
        set->Set.has("M") ? table->Map.set("M", index) : ()

        makeMaxIndexTable(table, index + 1)
      }
    }
  }

  let maxIndexTable = makeMaxIndexTable(Map.make(), 0)

  let rec collectGarbage = (totalTime: int, truck, index: int) => {
    switch index === Array.length(garbages) {
    | true => totalTime
    | false => {
        let garbage = switch garbages->Array.at(index) {
        | None => String.make()
        | Some(s) => s
        }
        let split = garbage->String.split("")
        let countTable = split->Array.reduce(Map.make(), (map, char) => {
          let count = switch map->Map.get(char) {
          | None => 1
          | Some(c) => c + 1
          }
          map->Map.set(char, count)
          map
        })
        let set = Set.fromArray(split)
        let travelTime = switch travelTimes->Array.get(index - 1) {
        | None => 0
        | Some(t) => t
        }

        switch truck {
        | G => {
            let maxIndex = switch maxIndexTable->Map.get("G") {
            | None => -1
            | Some(i) => i
            }
            let count = switch countTable->Map.get("G") {
            | None => 0
            | Some(c) => c
            }

            switch index > maxIndex {
            | true => totalTime
            | false =>
              switch set->Set.has("G") {
              | true => collectGarbage(totalTime + 1 * count + travelTime, truck, index + 1)
              | false => collectGarbage(totalTime + travelTime, truck, index + 1)
              }
            }
          }
        | P => {
            let maxIndex = switch maxIndexTable->Map.get("P") {
            | None => -1
            | Some(i) => i
            }
            let count = switch countTable->Map.get("P") {
            | None => 0
            | Some(c) => c
            }

            switch index > maxIndex {
            | true => totalTime
            | false =>
              switch set->Set.has("P") {
              | true => collectGarbage(totalTime + 1 * count + travelTime, truck, index + 1)
              | false => collectGarbage(totalTime + travelTime, truck, index + 1)
              }
            }
          }
        | M => {
            let maxIndex = switch maxIndexTable->Map.get("M") {
            | None => -1
            | Some(i) => i
            }
            let count = switch countTable->Map.get("M") {
            | None => 0
            | Some(c) => c
            }

            switch index > maxIndex {
            | true => totalTime
            | false =>
              switch set->Set.has("M") {
              | true => collectGarbage(totalTime + 1 * count + travelTime, truck, index + 1)
              | false => collectGarbage(totalTime + travelTime, truck, index + 1)
              }
            }
          }
        }
      }
    }
  }

  let glassTime = collectGarbage(0, G, 0)
  let paperTime = collectGarbage(0, P, 0)
  let metalTime = collectGarbage(0, M, 0)

  glassTime + paperTime + metalTime
}

let g1 = ["G", "P", "GP", "GG"]
let t1 = [2, 4, 3]
let r1 = minAmountOfTimeToCollectGarbage(g1, t1)
Console.log2("r1: ", r1) // 21

let g2 = ["MMM", "PGM", "GP"]
let t2 = [3, 10]
let r2 = minAmountOfTimeToCollectGarbage(g2, t2)
Console.log2("r2: ", r2) // 37
