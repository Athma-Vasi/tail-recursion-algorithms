type boxItem = | @as("#") Stone | @as("*") Obstacle | @as(".") Empty

let rotatingTheBox = (boxGrid: array<array<boxItem>>) => {
  let maxRows = Array.length(boxGrid)
  let row = switch boxGrid->Array.at(0) {
  | None => []
  | Some(r) => r
  }
  let maxColumns = Array.length(row)

  let rec rotateBox = (rotated: array<array<boxItem>>, rowIndex: int) => {
    switch rowIndex === maxRows {
    | true => rotated
    | false => {
        let row = switch boxGrid->Array.at(0) {
        | None => []
        | Some(r) => r
        }

        let rec columnLoop = (columnIndex: int) => {
          switch columnIndex === maxColumns {
          | true => ()
          | false => {
              let item = switch row->Array.at(columnIndex) {
              | None => Stone
              | Some(i) => i
              }
            }
          }
        }
      }
    }
  }
}
