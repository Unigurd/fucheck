import "src/futs/fucheck"
open Fucheck


entry maxtests (state : state) : maxtests =
  state.maxtests
entry maxsize  (state : state) : maxsize =
  state.maxsize
entry maxdiscardedratio (state : state) : maxdiscardedratio =
  state.maxdiscardedratio

let arbi32 (size : size) (seed : i32) : testdata (i32) =
  arbitraryi32.runGen size (minstd_rand.rng_from_seed [seed])

let arbtup (size : size) (seed : i32) : testdata (i32, i32) =
  (arbitrarytuple arbitraryi32 arbitraryi32).runGen size (minstd_rand.rng_from_seed [seed])

let genshit 't (gen : gen t) (size : size) (seed : i32) =
  gen.runGen size (minstd_rand.rng_from_seed [seed])

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
  case #testdata (i,j) -> show2tuple (showdecimali32) (showdecimali32) (i,j)

-- fucheck bool
entry boolarbitrary (size : size) (seed : i32) : testdata bool =
  arbitrarybool.runGen size (minstd_rand.rng_from_seed [seed])

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
  case #testdata (i,j) -> show2tuple (showdecimali32) (showdecimali32) (i,j)

entry condlabels (input : testdata (i32,i32)) : []u8 =
  match input
  case #testdata (i,j) -> "Difference of at least " ++ (showdecimali32 ((j - i) / 10 * 10))

-- fucheck zip
entry ziparbitrary (size : size) (seed : i32) : testdata ([](i32,i32)) =
  (arbitraryarr (arbitrarytuple arbitraryi32 arbitraryi32)).runGen
                                                           size
                                                           (minstd_rand.rng_from_seed [seed])
--  (arbitraryarr (arbitrarytuple arbitraryi32 arbitraryi32)).runGen size rng

entry zipproperty [n] (input : testdata ([n](i32,i32))) : bool =
  match input
  case #testdata arr -> arr == uncurry zip (unzip arr)

entry zipshow (input : testdata ([](i32,i32))) : []u8 =
  match input
  case #testdata arr -> showArray (show2tuple showdecimali32 showdecimali32) arr

-- fucheck badzip
entry badziparbitrary (size : size) (seed : i32) : testdata ([](i32,i32)) =
  (arbitraryarr (arbitrarytuple arbitraryi32 arbitraryi32)).runGen
                                                           size
                                                           (minstd_rand.rng_from_seed [seed])

entry badzipproperty [n] (input : testdata ([n](i32,i32))) : bool =
  match input
  case #testdata arr ->
    arr != uncurry zip (unzip arr)

entry badzipshow (input : testdata ([](i32,i32))) : []u8 =
  match input
  case #testdata arr -> showArray (show2tuple showdecimali32 showdecimali32) arr

-- fucheck failsize
entry failsizearbitrary (size : size) (seed : i32) : testdata ([]i32) =
  (arbitraryarr arbitraryi32).runGen
                             size
                             (minstd_rand.rng_from_seed [seed])

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
  (maybegen arbitraryi32).runGen size (minstd_rand.rng_from_seed [seed])

entry justfailproperty (input : testdata (maybe i32)) : bool =
  match input
  case #testdata (#just i)-> false
  case #testdata #nothing -> true

entry justfailshow (input : testdata (maybe i32)) : []u8 =
  maybeshow showdecimali32 (untestdata input)

-- fucheck nothingfail
entry nothingfailarbitrary (size : size) (seed : i32) : testdata (maybe i32) =
  (maybegen arbitraryi32).runGen size (minstd_rand.rng_from_seed [seed])

entry nothingfailproperty (input : testdata (maybe i32)) : bool =
  !(justfailproperty input)

entry nothingfailshow (input : testdata (maybe i32)) : []u8 =
  maybeshow showdecimali32 (untestdata input)

-- fucheck maybearr
entry maybearrarbitrary (size : size) (seed : i32) : testdata ([](maybe i32)) =
  (arbitraryarr (maybegen arbitraryi32)).runGen size (minstd_rand.rng_from_seed [seed])

entry maybearrproperty [n] (input : testdata ([n](maybe i32))) : bool =
  match input
  case #testdata arr -> length arr < 10

entry maybearrshow [n] (input : testdata ([n](maybe i32))) : []u8 =
  showArray (maybeshow showdecimali32) (untestdata input)

-- fucheck resizei32
entry resizei32arbitrary =
  genshit (resize (+10) arbitraryi32)

entry resizei32property (input : testdata i32) : bool = false

entry resizei32show (input : testdata i32) : []u8 =
  showdecimali32 (untestdata input)

-- fucheck resizearr
entry resizearrarbitrary [n] size seed : gen ([n]i32) =
  genshit (resize (+10) (arbitraryarr arbitraryi32)) size seed

entry resizearrproperty (input : testdata []i32) : bool = false

entry resizearrshow (input : testdata i32) : []u8 =
  (showArray showdecimali32) (untestdata input)
