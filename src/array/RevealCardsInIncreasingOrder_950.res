// T(n) = O(n * log(n))
// S(n) = O(n)

let revealCardsInIncreasingOrder = (deck: array<int>) => {
  let sorted = deck->Array.toSorted((c1, c2) => Int.compare(c1, c2))
  let length = Array.length(sorted)
  let half = length / 2

  let (smallerAsc, largerAsc) = sorted->Array.reduceWithIndex(([], []), (acc, card, idx) => {
    let (smaller, larger) = acc
    idx <= half ? (smaller->Array.concat([card]), larger) : (smaller, larger->Array.concat([card]))
  })

  let rec orderDeck = (ordered: array<int>, index: int) => {
    switch index > Array.length(largerAsc) {
    | true => ordered
    | false => {
        let smallerCard = switch smallerAsc->Array.at(index) {
        | None => 0
        | Some(c) => c
        }
        let largerCard = switch largerAsc->Array.at(index) {
        | None => 0
        | Some(c) => c
        }

        largerCard === 0
          ? orderDeck(ordered->Array.concat([smallerCard]), index + 1)
          : orderDeck(ordered->Array.concat([smallerCard, largerCard]), index + 1)
      }
    }
  }

  Array.length(deck) < 3 ? deck->Array.toSorted((c1, c2) => Int.compare(c1, c2)) : orderDeck([], 0)
}

let d1 = [17, 13, 11, 2, 3, 5, 7]
let r1 = revealCardsInIncreasingOrder(d1)
Console.log2("r1: ", r1) // [2,13,3,11,5,17,7]

let d2 = [1, 1000]
let r2 = revealCardsInIncreasingOrder(d2)
Console.log2("r2: ", r2) // [1,1000]
