// T(n) = O(n)
// S(n) = O(n)

let asteroidCollision = (asteroids: array<int>) => {
  let rec updateStack = (stack: array<int>, asteroid: int) => {
    let prevAsteroid = switch stack->Array.at(-1) {
    | None => Int32.min_int
    | Some(ast) => ast
    }
    let stackLength = Array.length(stack)

    let areAsteroidsSameDirection =
      (prevAsteroid < 1 && asteroid < 1) || (prevAsteroid > 1 && asteroid > 1)

    switch areAsteroidsSameDirection || stackLength === 0 {
    | true => stack->Array.concat([asteroid])
    | false => {
        let absPrevAsteroid = prevAsteroid < 1 ? prevAsteroid * -1 : prevAsteroid
        let absAsteroid = asteroid < 1 ? asteroid * -1 : asteroid

        absPrevAsteroid > absAsteroid
          ? stack
          : absPrevAsteroid === absAsteroid
          ? stack->Array.slice(~start=0, ~end=stackLength - 1)
          : updateStack(stack->Array.slice(~start=0, ~end=stackLength - 1), asteroid)
      }
    }
  }

  let rec loop = (stack: array<int>, index: int) => {
    switch index === Array.length(asteroids) {
    | true => stack
    | false => {
        let asteroid = switch asteroids->Array.at(index) {
        | None => Int32.min_int
        | Some(ast) => ast
        }

        loop(updateStack(stack, asteroid), index + 1)
      }
    }
  }

  loop([], 0)
}

let a1 = [5, 10, -5]
let r1 = asteroidCollision(a1)
Console.log2("r1: ", r1) // [5, 10]

let a2 = [8, -8]
let r2 = asteroidCollision(a2)
Console.log2("r2: ", r2) // []

let a3 = [10, 2, -5]
let r3 = asteroidCollision(a3)
Console.log2("r3: ", r3) // [10]
