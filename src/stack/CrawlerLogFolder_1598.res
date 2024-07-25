// T(n) = O(n)
// S(n) = O(n)

let crawlerLogFolder = (logs: array<string>) => {
  let length = Array.length(logs)

  let rec loop = (folders: array<string>, index: int) => {
    let foldersLength = Array.length(folders)

    switch index === length {
    | true => foldersLength
    | false => {
        let log = switch logs->Array.get(index) {
        | None => ""
        | Some(l) => l
        }

        switch log {
        | "../" => loop(folders->Array.slice(~start=0, ~end=foldersLength - 1), index + 1)
        | "./" => loop(folders, index + 1)
        | _ => loop(folders->Array.concat([log]), index + 1)
        }
      }
    }
  }

  loop([], 0)
}

let l1 = ["d1/", "d2/", "../", "d21/", "./"]
let r1 = crawlerLogFolder(l1)
Console.log2("r1: ", r1)

let l2 = ["d1/", "d2/", "./", "d3/", "../", "d31/"]
let r2 = crawlerLogFolder(l2)
Console.log2("r2: ", r2)

let l3 = ["d1/", "../", "../", "../"]
let r3 = crawlerLogFolder(l3)
Console.log2("r3: ", r3)
