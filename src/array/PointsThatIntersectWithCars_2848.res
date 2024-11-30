let pointsThatIntersectWithCars = (nums: array<(int, int)>) => {
  let rec collectRange = (intersections: Set.t<int>, start: int, end: int) => {
    switch start === end + 1 {
    | true => intersections
    | false => {
        intersections->Set.add(start)

        collectRange(intersections, start + 1, end)
      }
    }
  }

  let rec findIntersectionPoints = (intersections: Set.t<int>, index: int) => {
    switch index === Array.length(nums) {
    | true => Set.size(intersections)
    | false => {
        let (start, end) = switch nums->Array.at(index) {
        | None => (0, 0)
        | Some(c) => c
        }

        findIntersectionPoints(collectRange(intersections, start, end), index + 1)
      }
    }
  }

  findIntersectionPoints(Set.make(), 0)
}

let n1 = [(3, 6), (1, 5), (4, 7)]
let r1 = pointsThatIntersectWithCars(n1)
Console.log2("r1: ", r1) // 7

let n2 = [(1, 3), (5, 8)]
let r2 = pointsThatIntersectWithCars(n2)
Console.log2("r2: ", r2) // 7
