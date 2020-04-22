import "src/futs/fucheck"
open Fucheck

entry maxtests (state : state) : maxtests =
  state.maxtests
entry maxsize  (state : state) : maxsize =
  state.maxsize
entry maxdiscardedratio (state : state) : maxdiscardedratio =
  state.maxdiscardedratio

let arbi32 (size : size) (seed : i32) : testdata (i32) =
  runGen arbitraryi32 size (minstd_rand.rng_from_seed [seed])

let arbtup (size : size) (seed : i32) : testdata (i32, i32) =
  runGen (arbitrarytuple arbitraryi32 arbitraryi32) size (minstd_rand.rng_from_seed [seed])

-- fucheck pass
entry passarbitrary = arbi32

  --runGen (arbitrarytuple arbitraryi32 arbitraryi32) size (minstd_rand.rng_from_seed [seed])
entry passproperty (input : testdata i32) : bool = match input
  case #testdata i -> i == i

entry passshow (input : testdata i32) : []u8 = match input
  case #testdata i -> showdecimali32 i

entry passstate : state = { maxtests = 305 , maxsize = 513, maxdiscardedratio = 100 }

-- fucheck failWithShow
entry failWithShowarbitrary = arbi32

entry failWithShowproperty (input : testdata i32) = match input
  case #testdata i -> i != i

entry failWithShowshow (input : testdata i32) : []u8 = match input
  case #testdata m -> showdecimali32 m

-- fucheck failWithoutShow
entry failWithoutShowarbitrary = arbi32

entry failWithoutShowproperty (input : testdata i32) = match input
  case #testdata i -> i != i

-- fucheck tupleMightFail
entry tupleMightFailarbitrary = arbtup

entry tupleMightFailproperty (input : testdata (i32, i32)) =
  match input
  case #testdata (i,j) -> i == j


entry tupleMightFailshow (input : testdata (i32,i32)) : []u8 =
  match input
  case #testdata (i,j) -> show2tuple (showdecimali32 i) (showdecimali32 j)

-- fucheck bool

entry boolarbitrary (size : size) (seed : i32) : testdata bool =
  runGen arbitrarybool size (minstd_rand.rng_from_seed [seed])

entry boolproperty (input : testdata bool) =
  match input
  case #testdata b -> b

entry boolshow (input : testdata bool) =
  match input
  case #testdata b -> showbool b

entry boolstate : state = { maxtests = 55 , maxsize = 101, maxdiscardedratio = 100 }


-- fucheck cond

entry condstate = { maxtests = 157 , maxsize = 131, maxdiscardedratio = 100 }

entry condarbitrary = arbtup

entry condcondition (input : testdata (i32, i32)) =
  match input
  case #testdata (i,j) -> i <= j

entry condproperty (input : testdata (i32, i32)) =
  match input
  case #testdata (i,j) -> i <= j


entry condshow (input : testdata (i32,i32)) : []u8 =
  match input
  case #testdata (i,j) -> show2tuple (showdecimali32 i) (showdecimali32 j)



entry condlabels (input : testdata (i32,i32)) : []u8 =
  match input
  case #testdata (i,j) -> "Difference of at least " ++ (showdecimali32 ((j - i) / 10 * 10))





---- fucheck zip
--let ziparbitrary (rng : rng) : ([]i32, []i32) =
--  let (rng, length) = dist.rand (0,1000) rng
--  let (rng, arr1)   = rngArrLen (rngi32range (-100,100)) length rng
--  let (_,    arr2)  = rngArrLen (rngi32range (-100,100)) length rng
--  in (arr1, arr2)
--
--let zipproperty [n] ((as,bs) : ([n]i32,[n]i32)) = (as,bs) == unzip (zip as bs)
--
--let zipshow _ : []u8 = "zipShow not implemented"
