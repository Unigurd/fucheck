type result = #success | #failure i32
type seed = i32

let overFailure f (result : result) : result =
  match result
  case #success -> #success
  case failure  -> f failure

let bool2result (seed : seed) (b : bool) : result =
  match b
  case true  -> #success
  case false -> #failure seed

let mappend (result1 : result) (result2 : result) : result =
  match (result1, result2)
  case (#failure i, _) -> #failure i
  case (_, rhs)           -> rhs

let addAssoc ((a,b,c) : (i32,i32,i32)) : bool = ((a + b) + c) == (a + (b + c))

let gen (a : i32) : (i32,i32,i32) = (a, a+1, a+2)

let runTest 'a (test : (a -> bool)) (gen : i32 -> a) (seed : seed) : bool = test (gen seed)

entry test_entry_result (seed : seed) : result = bool2result seed <| runTest addAssoc gen seed

entry test_entry (seed : seed) : bool = runTest addAssoc gen seed
