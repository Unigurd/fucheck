import "src/futs/fucheck"
open Fucheck

let defaultstate : state = {maxtests = 100, maxsize = 100, maxdiscardedratio = 10}

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


-- fucheck zip
entry ziparbitrary (maxsize : size) (seed : i32) : testdata ([]i32, []i32) =
  let rng = rng_from_seed seed
  let (sizes,newrng) = getsizes maxsize rng 1
  let mysize = sizes[0]
  let my_arb = (arbitraryarr arbitraryi32 mysize)
  in runGen (arbitrarytuple my_arb my_arb) maxsize newrng

entry zipproperty [n] (input : testdata ([n]i32,[n]i32)) : bool =
  match input
  case #testdata (as,bs) -> true --(as,bs) == unzip (zip as bs)


-- fucheck transpose
entry transposearbitrary (size : size) (seed : i32) : testdata ([][]i32) =
  let rng = rng_from_seed seed
  let (sizes,newrng) = getsizes (size+10) rng 2
  let my_arb = resize (10+) (arbitrary2darr arbitraryi32 sizes[0] sizes[1])
  in runGen my_arb size newrng


entry transposeproperty [n] [m] (input : testdata ([n][m]i32)) : bool =
  match input
  case #testdata m -> m != transpose (transpose m)

entry transposeshow [n] [m] (input : testdata ([n][m]i32)) : []u8 =
  match input
  case #testdata arr ->
    showArray2d showdecimali32 arr

-- fucheck d3
entry d3arbitrary (size : size) (seed : i32) : testdata ([][][]i32) =
  let rng = rng_from_seed seed
  let (sizes,newrng) = getsizes (size+10) rng 3
  let my_arb = resize (+10) (arbitrary3darr arbitraryi32 sizes[0] sizes[1] sizes[2])
  in runGen my_arb size newrng

entry d3property  (input : testdata ([][][]i32)) : bool =
  match input
  case #testdata m -> m != transpose (transpose m)

entry d3show (input : testdata ([][][]i32)) : []u8 =
  match input case #testdata arr ->
    showArray3d showdecimali32 arr

-- fucheck d4
entry d4arbitrary (size : size) (seed : i32) : testdata ([][][][]i32) =
  let rng = rng_from_seed seed
  let (sizes,newrng) = getsizes (size+10) rng 4
  let my_arb = resize (+10) (arbitrary4darr arbitraryi32 sizes[0] sizes[1] sizes[2] sizes[3])
  in runGen my_arb size newrng

entry d4property  (input : testdata ([][][][]i32)) : bool =
  match input
  case #testdata m -> m != transpose (transpose m)

entry d4show (input : testdata ([][][][]i32)) : []u8 =
  match input case #testdata arr ->
    showArray4d showdecimali32 arr

entry d4state : state = {maxtests = 100, maxdiscardedratio = 10, maxsize = 20}

-- fucheck d5
entry d5arbitrary (size : size) (seed : i32) : testdata ([][][][][]i32) =
  let rng = rng_from_seed seed
  let (sizes,newrng) = getsizes size rng 5
  let my_arb = (arbitrary5darr arbitraryi32 sizes[0] sizes[1] sizes[2] sizes[3] sizes[4])
  in runGen my_arb size newrng

entry d5property  (input : testdata ([][][][][]i32)) : bool =
  match input
  case #testdata m -> m == transpose (transpose m)

entry d5show (input : testdata ([][][][][]i32)) : []u8 =
  match input case #testdata arr ->
    showArray5d showdecimali32 arr

entry d5state = {maxdiscardedratio = 10, maxtests = 100, maxsize = 10}
