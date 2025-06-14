type rec t<'a> = {val: 'a, branches: option<list<t<'a>>>}

let make = (~val: 'a, ~branches=None) => {
  {
    val,
    branches,
  }
}
