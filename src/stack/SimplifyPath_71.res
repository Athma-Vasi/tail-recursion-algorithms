// T(n) = O(n)
// S(n) = O(n)

let simplifyPath = (path: string) => {
  let pathArray = String.split(path, "/")->Array.filter(v => String.length(v) > 0)

  let rec loop = (pathStack: string, index: int) => {
    let stackLength = String.length(pathStack)
    switch index === Array.length(pathArray) {
    | true => pathStack
    | false => {
        let command = switch pathArray->Array.at(index) {
        | None => ""
        | Some(co) => co
        }
        let previous = pathStack->String.charAt(stackLength - 1)

        switch command {
        | "." => loop(pathStack, index + 1)
        | ".." =>
          loop(
            previous === "/"
              ? pathStack
              : String.length(previous) === 0
              ? "/"
              : pathStack->String.slice(~start=0, ~end=stackLength - 2),
            index + 1,
          )
        | folder =>
          loop(pathStack->String.concat(previous === "/" ? folder : "/" ++ folder), index + 1)
        }
      }
    }
  }

  loop("", 0)
}

let p1 = "/home/"
let r1 = simplifyPath(p1)
Console.log3("r1: ", r1, r1 === "/home")

let p2 = "/../"
let r2 = simplifyPath(p2)
Console.log3("r2: ", r2, r2 === "/")

let p3 = "/home//foo/"
let r3 = simplifyPath(p3)
Console.log3("r3: ", r3, r3 === "/home/foo")

let p4 = "/a/./b/../../c/"
let r4 = simplifyPath(p4)
Console.log3("r4: ", r4, r4 === "/c")
