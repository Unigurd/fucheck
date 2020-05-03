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
  case #testdata t -> show2tuple showdecimali32 showdecimali32 t

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
  case #testdata t -> show2tuple showdecimali32 showdecimali32 t



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
  case #testdata m -> m == transpose (transpose m)

entry transposeshow [n] [m] (input : testdata ([n][m]i32)) : []u8 =
  match input
  case #testdata arr ->
    showArray2d showdecimali32 arr

---- fucheck d3
--entry d3arbitrary (size : size) (seed : i32) : testdata ([][][]i32) =
--  let rng = rng_from_seed seed
--  let (sizes,newrng) = getsizes (size+10) rng 3
--  let my_arb = resize (+10) (arbitrary3darr arbitraryi32 sizes[0] sizes[1] sizes[2])
--  in runGen my_arb size newrng
--
--entry d3property  (input : testdata ([][][]i32)) : bool =
--  match input
--  case #testdata m -> m == transpose (transpose m)
--
--entry d3show (input : testdata ([][][]i32)) : []u8 =
--  match input case #testdata arr ->
--    showArray3d showdecimali32 arr
--
---- fucheck d4
--entry d4arbitrary (size : size) (seed : i32) : testdata ([][][][]i32) =
--  let rng = rng_from_seed seed
--  let (sizes,newrng) = getsizes (size+10) rng 4
--  let my_arb = resize (+10) (arbitrary4darr arbitraryi32 sizes[0] sizes[1] sizes[2] sizes[3])
--  in runGen my_arb size newrng
--
--entry d4property  (input : testdata ([][][][]i32)) : bool =
--  match input
--  case #testdata m -> m == transpose (transpose m)
--
--entry d4show (input : testdata ([][][][]i32)) : []u8 =
--  match input case #testdata arr ->
--    showArray4d showdecimali32 arr
--
--entry d4state : state = {maxtests = 100, maxdiscardedratio = 10, maxsize = 20}

---- fucheck d5
--entry d5arbitrary (size : size) (seed : i32) : testdata ([][][][][]i32) =
--  let rng = rng_from_seed seed
--  let (sizes,newrng) = getsizes size rng 5
--  let my_arb = (arbitrary5darr arbitraryi32 sizes[0] sizes[1] sizes[2] sizes[3] sizes[4])
--  in runGen my_arb size newrng
--
--entry d5property  (input : testdata ([][][][][]i32)) : bool =
--  match input
--  case #testdata m -> m == transpose (transpose m)
--
--entry d5show (input : testdata ([][][][][]i32)) : []u8 =
--  match input case #testdata arr ->
--    showArray5d showdecimali32 arr
--
--entry d5state = {maxdiscardedratio = 10, maxtests = 100, maxsize = 10}

-- fucheck badzip
entry badziparbitrary (size : size) (seed : i32) : testdata ([](i32,i32)) =
  let rng = rng_from_seed seed
  let (sizes,newrng) = getsizes size rng 1
  let my_arb = (arbitraryarr (arbitrarytuple arbitraryi32 arbitraryi32) sizes[0])
  in runGen my_arb size newrng

entry badzipproperty [n] (input : testdata ([n](i32,i32))) : bool =
  match input
  case #testdata arr ->
    arr != uncurry zip (unzip arr)

entry badzipshow (input : testdata ([](i32,i32))) : []u8 =
  match input
  case #testdata arr -> showArray (show2tuple showdecimali32 showdecimali32) arr

-- fucheck failsize
entry failsizearbitrary (size : size) (seed : i32) : testdata ([]i32) =
  let rng = rng_from_seed seed
  let (sizes,newrng) = getsizes size rng 1
  let my_arb = (arbitraryarr arbitraryi32 sizes[0])
  in runGen my_arb size newrng

--uncurry (arbitraryarr arbitraryi32 size) <| start size seed

entry failsizeproperty [n] (input : testdata ([n]i32)) : bool =
  match input
  case #testdata arr -> length arr < 10

entry failsizeshow (input : testdata ([]i32)) : []u8 =
  match input
  case #testdata arr -> showArray showdecimali32 arr

type maybe 'a = #just a | #nothing
let maybegen 't elmgen : gen (maybe t) =
  oneof2 (transformgen (\i -> #just i) elmgen)
         (constgen #nothing)

let maybeshow 't (elmshow : t -> []u8) (input : (maybe t)) : []u8 =
  match input
  case (#just i) -> "#just " ++ elmshow i
  case #nothing -> "#nothing"

-- fucheck justfail
entry justfailarbitrary (size : size) (seed : i32) : testdata (maybe i32) =
  let rng = rng_from_seed seed
  in runGen (maybegen arbitraryi32) size rng

entry justfailproperty (input : testdata (maybe i32)) : bool =
  match input
  case #testdata (#just i)-> false
  case #testdata #nothing -> true

entry justfailshow (input : testdata (maybe i32)) : []u8 =
  maybeshow showdecimali32 (untestdata input)

-- fucheck nothingfail
entry nothingfailarbitrary (size : size) (seed : i32) : testdata (maybe i32) =
  let rng = rng_from_seed seed
  in runGen (maybegen arbitraryi32) size rng

entry nothingfailproperty (input : testdata (maybe i32)) : bool =
  !(justfailproperty input)

entry nothingfailshow (input : testdata (maybe i32)) : []u8 =
  maybeshow showdecimali32 (untestdata input)

-- fucheck maybearr
entry maybearrarbitrary (size : size) (seed : i32) : testdata ([](maybe i32)) =
  let rng = rng_from_seed seed
  let (sizes,newrng) = getsizes size rng 1
  let my_arb = (arbitraryarr (maybegen arbitraryi32) sizes[0])
  in runGen my_arb size newrng

entry maybearrproperty [n] (input : testdata ([n](maybe i32))) : bool =
  match input
  case #testdata arr -> length arr < 10

entry maybearrshow [n] (input : testdata ([n](maybe i32))) : []u8 =
  showArray (maybeshow showdecimali32) (untestdata input)

-- fucheck resizei32
entry resizei32arbitrary (size : size) (seed : i32) =
  let rng = rng_from_seed seed
  in runGen (scale (+10) arbitraryi32) size rng

entry resizei32property (input : testdata i32) : bool = false

entry resizei32show (input : testdata i32) : []u8 =
  showdecimali32 (untestdata input)
