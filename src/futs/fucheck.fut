import "test"
type result = #success | #failure i32
type seed = i32

let bool2result (seed : seed) (b : bool) : result =
  match b
  case true  -> #success
  case false -> #failure seed




entry runTest (seed : seed) : bool = Test.property (Test.gen seed)

