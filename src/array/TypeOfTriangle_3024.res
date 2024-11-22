// T(n) = O(n)
// S(n) = O(n)

type triangle =
  | @as("equilateral") Equilateral
  | @as("isosceles") Isosceles
  | @as("scalene") Scalene
  | @as("none") None

let typeOfTriangle = (sides: array<int>): triangle => {
  let checkIsTriangle = sides => {
    let aSide = switch sides->Array.at(0) {
    | None => 0
    | Some(s) => s
    }
    let bSide = switch sides->Array.at(1) {
    | None => 0
    | Some(s) => s
    }
    let cSide = switch sides->Array.at(2) {
    | None => 0
    | Some(s) => s
    }

    let case1 = aSide + bSide > cSide
    let case2 = aSide + cSide > bSide
    let case3 = bSide + cSide > aSide

    case1 && case2 && case3
  }

  let checkType = sides => {
    let sidesSet = sides->Array.reduce(Set.make(), (set, side) => {
      set->Set.add(side)
      set
    })

    switch Set.size(sidesSet) {
    | 1 => Equilateral
    | 2 => Isosceles
    | 3 => Scalene
    | _ => None
    }
  }

  checkIsTriangle(sides) ? checkType(sides) : None
}

let s1 = [3, 3, 3]
let r1 = typeOfTriangle(s1)
Console.log2("r1: ", r1)

let s2 = [3, 4, 5]
let r2 = typeOfTriangle(s2)
Console.log2("r2: ", r2)
