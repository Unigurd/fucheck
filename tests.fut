import "/home/sigurd/studie/bachelor/fucheck/src/futs/fucheck"
open Fucheck

let arbi32 (size : size) (seed : i32) : testdata (i32) =
  arbitraryi32 size <| rng_from_seed seed

let arbtup (size : size) (seed : i32) : testdata (i32, i32) =
  (arbitrarytuple arbitraryi32 arbitraryi32) size <| rng_from_seed seed


-- fucheck pass
entry gen_pass = arbi32

  --(arbitrarytuple arbitraryi32 arbitraryi32) size <| rng_from_seed seed
entry prop_pass (input : testdata i32) : bool = match input
  case #testdata i -> i == i

entry show_pass (input : testdata i32) : []u8 = match input
  case #testdata i -> showdecimali32 i

entry state_pass : state = { maxtests = 305 , maxsize = 513, maxdiscardedratio = 100 }

-- fucheck failWithShow
entry gen_failWithShow = arbi32

entry prop_failWithShow (input : testdata i32) = match input
  case #testdata i -> i != i

entry show_failWithShow (input : testdata i32) : []u8 = match input
  case #testdata m -> showdecimali32 m

-- fucheck failWithoutShow
entry gen_failWithoutShow = arbi32

entry prop_failWithoutShow (input : testdata i32) = match input
  case #testdata i -> i != i

-- fucheck tupleMightFail
entry gen_tupleMightFail = arbtup

entry prop_tupleMightFail (input : testdata (i32, i32)) =
  match input
  case #testdata (i,j) -> i == j


entry show_tupleMightFail (input : testdata (i32,i32)) : []u8 =
  match input
  case #testdata t -> show2tuple showdecimali32 showdecimali32 t

-- fucheck bool
entry gen_bool (size : size) (seed : i32) : testdata bool =
  arbitrarybool size <| rng_from_seed seed

entry prop_bool (input : testdata bool) =
  match input
  case #testdata b -> b

entry show_bool (input : testdata bool) =
  match input
  case #testdata b -> showbool b

entry state_bool : state = { maxtests = 55 , maxsize = 101, maxdiscardedratio = 100 }


-- fucheck cond

entry state_cond = { maxtests = 157 , maxsize = 131, maxdiscardedratio = 100 }

entry gen_cond = arbtup

entry cond_cond (input : testdata (i32, i32)) =
  match input
  case #testdata (i,j) -> i <= j

entry prop_cond (input : testdata (i32, i32)) =
  match input
  case #testdata (i,j) -> i <= j


entry show_cond (input : testdata (i32,i32)) : []u8 =
  match input
  case #testdata t -> show2tuple showdecimali32 showdecimali32 t


entry labels_cond (input : testdata (i32,i32)) : []u8 =
  match input
  case #testdata (i,j) -> "Difference of at least " ++ (showdecimali32 ((j - i) / 10 * 10))


-- fucheck zip
entry gen_zip (maxsize : size) (seed : i32) : testdata ([]i32, []i32) =
  let rngs = split_rng 2 <| rng_from_seed seed
  let sizes = getsizes maxsize rngs[0] 1
  let mysize = sizes[0]
  let my_arb = (arbitraryarr arbitraryi32 mysize)
  in (arbitrarytuple my_arb my_arb) maxsize rngs[1]

entry prop_zip [n] (input : testdata ([n]i32,[n]i32)) : bool =
  match input
  case #testdata (as,bs) -> true --(as,bs) == unzip (zip as bs)


-- fucheck transpose
entry gen_transpose (size : size) (seed : i32) : testdata ([][]i32) =
  let rngs = split_rng 2 <| rng_from_seed seed
  let sizes = getsizes (size+10) rngs[0] 2
  let my_arb = scale (10+) (arbitrary2darr arbitraryi32 sizes[0] sizes[1])
  in my_arb size rngs[1]


entry prop_transpose [n] [m] (input : testdata ([n][m]i32)) : bool =
  match input
  case #testdata m -> m == transpose (transpose m)

entry show_transpose [n] [m] (input : testdata ([n][m]i32)) : []u8 =
  match input
  case #testdata arr ->
    showArray2d showdecimali32 arr

---- fucheck d3
--entry gen_d3 (size : size) (seed : i32) : testdata ([][][]i32) =
--  let rng = rng_from_seed seed
--  let (sizes,newrng) = getsizes (size+10) rng 3
--  let my_arb = scale (+10) (arbitrary3darr arbitraryi32 sizes[0] sizes[1] sizes[2])
--  in my_arb size newrng
--
--entry prop_d3 (input : testdata ([][][]i32)) : bool =
--  match input
--  case #testdata m -> m == transpose (transpose m)
--
--entry show_d3 (input : testdata ([][][]i32)) : []u8 =
--  match input case #testdata arr ->
--    showArray3d showdecimali32 arr
--
---- fucheck d4
--entry gen_d4 (size : size) (seed : i32) : testdata ([][][][]i32) =
--  let rng = rng_from_seed seed
--  let (sizes,newrng) = getsizes (size+10) rng 4
--  let my_arb = scale (+10) (arbitrary4darr arbitraryi32 sizes[0] sizes[1] sizes[2] sizes[3])
--  in my_arb size newrng
--
--entry prop_d4 (input : testdata ([][][][]i32)) : bool =
--  match input
--  case #testdata m -> m == transpose (transpose m)
--
--entry show_d4 (input : testdata ([][][][]i32)) : []u8 =
--  match input case #testdata arr ->
--    showArray4d showdecimali32 arr
--
--entry state_d4 : state = {maxtests = 100, maxdiscardedratio = 10, maxsize = 20}

---- fucheck d5
--entry gen_d5 (size : size) (seed : i32) : testdata ([][][][][]i32) =
--  let rng = rng_from_seed seed
--  let (sizes,newrng) = getsizes size rng 5
--  let my_arb = (arbitrary5darr arbitraryi32 sizes[0] sizes[1] sizes[2] sizes[3] sizes[4])
--  in my_arb size newrng
--
--entry prop_d5 (input : testdata ([][][][][]i32)) : bool =
--  match input
--  case #testdata m -> m == transpose (transpose m)
--
--entry show_d5 (input : testdata ([][][][][]i32)) : []u8 =
--  match input case #testdata arr ->
--    showArray5d showdecimali32 arr
--
--entry state_d5 = {maxdiscardedratio = 10, maxtests = 100, maxsize = 10}

-- fucheck badzip
entry gen_badzip (size : size) (seed : i32) : testdata ([](i32,i32)) =
  let rngs = split_rng 2 <| rng_from_seed seed
  let sizes = getsizes size rngs[0] 1
  let my_arb = (arbitraryarr (arbitrarytuple arbitraryi32 arbitraryi32) sizes[0])
  in my_arb size rngs[1]

entry prop_badzip [n] (input : testdata ([n](i32,i32))) : bool =
  match input
  case #testdata arr ->
    arr != uncurry zip (unzip arr)

entry show_badzip (input : testdata ([](i32,i32))) : []u8 =
  match input
  case #testdata arr -> showArray (show2tuple showdecimali32 showdecimali32) arr

-- fucheck failsize
entry gen_failsize (size : size) (seed : i32) : testdata ([]i32) =
  let rngs = split_rng 2 <| rng_from_seed seed
  let sizes = getsizes size rngs[0] 1
  let my_arb = (arbitraryarr arbitraryi32 sizes[0])
  in my_arb size rngs[1]

--uncurry (arbitraryarr arbitraryi32 size) <| start size seed

entry prop_failsize [n] (input : testdata ([n]i32)) : bool =
  match input
  case #testdata arr -> length arr < 10

entry show_failsize (input : testdata ([]i32)) : []u8 =
  match input
  case #testdata arr -> showArray showdecimali32 arr

type maybe 'a = #just a | #nothing
let maybegen 't elmgen : gen (maybe t) =
  oneof2 (transformgen (\i -> #just i) elmgen)
         (constgen #nothing)

let show_maybe 't (elmshow : t -> []u8) (input : (maybe t)) : []u8 =
  match input
  case (#just i) -> "#just " ++ elmshow i
  case #nothing -> "#nothing"

-- fucheck justfail
entry gen_justfail (size : size) (seed : i32) : testdata (maybe i32) =
  let rng = rng_from_seed seed
  in (maybegen arbitraryi32) size rng

entry prop_justfail (input : testdata (maybe i32)) : bool =
  match input
  case #testdata (#just i)-> false
  case #testdata #nothing -> true

entry show_justfail (input : testdata (maybe i32)) : []u8 =
  show_maybe showdecimali32 (untestdata input)

-- fucheck nothingfail
entry gen_nothingfail (size : size) (seed : i32) : testdata (maybe i32) =
  let rng = rng_from_seed seed
  in (maybegen arbitraryi32) size rng

entry prop_nothingfail (input : testdata (maybe i32)) : bool =
  !(prop_justfail input)

entry show_nothingfail (input : testdata (maybe i32)) : []u8 =
  show_maybe showdecimali32 (untestdata input)

-- fucheck maybearr
entry gen_maybearr (size : size) (seed : i32) : testdata ([](maybe i32)) =
  let rngs = split_rng 2 <| rng_from_seed seed
  let sizes = getsizes size rngs[0] 1
  let my_arb = (arbitraryarr (maybegen arbitraryi32) sizes[0])
  in my_arb size rngs[1]

entry prop_maybearr [n] (input : testdata ([n](maybe i32))) : bool =
  match input
  case #testdata arr -> length arr < 10

entry show_maybearr [n] (input : testdata ([n](maybe i32))) : []u8 =
  showArray (show_maybe showdecimali32) (untestdata input)
