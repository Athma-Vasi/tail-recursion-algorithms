type rec t<'a> = {val: 'a, mutable left: option<t<'a>>, mutable right: option<t<'a>>}

let make = (~val: 'a, ~left=None, ~right=None): t<'a> => {
  {
    val,
    left,
    right,
  }
}
