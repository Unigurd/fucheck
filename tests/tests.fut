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

entry show_pass (input : testdata i32) : []u8 = showdecimali32 input

entry state_pass : state = { maxtests = 305 , maxsize = 513, maxdiscardedratio = 100 }

-- fucheck failWithShow
entry gen_failWithShow = arbi32

entry prop_failWithShow (input : testdata i32) = match input
  case #testdata i -> i != i

entry show_failWithShow (input : testdata i32) : []u8 = showdecimali32 input

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
show2tuple showdecimali32 showdecimali32 input

-- fucheck bool
entry gen_bool (size : size) (seed : i32) : testdata bool =
  arbitrarybool size <| rng_from_seed seed

entry prop_bool (input : testdata bool) =
  match input
  case #testdata b -> b

entry show_bool (input : testdata bool) =
  showbool input

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
  show2tuple showdecimali32 showdecimali32 input


entry labels_cond (input : testdata (i32,i32)) : []u8 =
  match input
  case #testdata (i,j) -> "Difference of at least " ++ (showdecimali32 <| testdata ((j - i) / 10 * 10))


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
  show_array2d showdecimali32 input

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
  show_array (show2tuple showdecimali32 showdecimali32) input

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
  show_array showdecimali32 input

type maybe 'a = #just a | #nothing
let maybegen 't elmgen : gen (maybe t) =
  oneof2 (transformgen (\i -> #just i) elmgen)
         (constgen #nothing)

let show_maybe 't (elmshow : testdata t -> []u8) (input : testdata (maybe t)) : []u8 =
  match input
  case #testdata (#just i) -> "#just " ++ elmshow (testdata i)
  case #testdata #nothing -> "#nothing"

-- fucheck justfail
entry gen_justfail (size : size) (seed : i32) : testdata (maybe i32) =
  let rng = rng_from_seed seed
  in (maybegen arbitraryi32) size rng

entry prop_justfail (input : testdata (maybe i32)) : bool =
  match input
  case #testdata (#just i)-> false
  case #testdata #nothing -> true

entry show_justfail (input : testdata (maybe i32)) : []u8 =
  show_maybe showdecimali32 input

-- fucheck nothingfail
entry gen_nothingfail (size : size) (seed : i32) : testdata (maybe i32) =
  let rng = rng_from_seed seed
  in (maybegen arbitraryi32) size rng

entry prop_nothingfail (input : testdata (maybe i32)) : bool =
  !(prop_justfail input)

entry show_nothingfail (input : testdata (maybe i32)) : []u8 =
  show_maybe showdecimali32 input

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
  show_array (show_maybe showdecimali32) input


-- fucheck show_crash
let gen_show_crash = arbi32
let prop_show_crash (input : testdata i32) =
  match input
  case #testdata i -> false
--    let c = i/0
--    in if c != 0 then false else true
let show_show_crash (input : testdata i32) =
  match input
  case #testdata i ->
    let c = i/0
    in showdecimali32 <| testdata c

-- fucheck rev_id
let gen_rev_id size seed =
  let rngs = split_rng 2 <| rng_from_seed seed
  let sizes = getsizes size rngs[0] 1
  in arbitraryarr arbitraryi32 sizes[0] size rngs[1]

let prop_rev_id (input : testdata ([]i32)) =
  match input
  case #testdata arr -> reverse (reverse arr) == arr



-- fucheck show_2d_arr
let gen_show_2d_arr (size : size) (seed : i32) : testdata ([][]i32) =
  let rngs = split_rng 2 <| rng_from_seed seed
  let sizes = getsizes size rngs[0] 5
  let my_arb = (arbitrary2darr arbitraryi32 sizes[0] sizes[1])
  in my_arb size rngs[1]

let prop_show_2d_arr (input : testdata ([][]i32)) =
  match input
  case #testdata arr -> length arr <= 2

let show_show_2d_arr (input : testdata ([][]i32)) = show_array2d showdecimali32 input

-- fucheck show_3d_arr
let gen_show_3d_arr (size : size) (seed : i32) : testdata ([][][]i32) =
  let rngs = split_rng 2 <| rng_from_seed seed
  let sizes = getsizes size rngs[0] 5
  let my_arb = (arbitrary3darr arbitraryi32 sizes[0] sizes[1] sizes[2])
  in my_arb size rngs[1]

let prop_show_3d_arr (input : testdata ([][][]i32)) =
  match input
  case #testdata arr -> length arr <= 2

let show_show_3d_arr (input : testdata ([][][]i32)) = show_array3d showdecimali32 input

 -- fucheck show_4d_arr
 let gen_show_4d_arr (size : size) (seed : i32) : testdata ([][][][]i32) =
   let rngs = split_rng 2 <| rng_from_seed seed
   let sizes = getsizes size rngs[0] 5
   let my_arb = (arbitrary4darr arbitraryi32 sizes[0] sizes[1] sizes[2] sizes[3])
   in my_arb size rngs[1]

 let prop_show_4d_arr (input : testdata ([][][][]i32)) =
   match input
   case #testdata arr -> length arr <= 2

 let show_show_4d_arr (input : testdata ([][][][]i32)) = show_array4d showdecimali32 input

-- fucheck show_5d_arr
let gen_show_5d_arr (size : size) (seed : i32) : testdata ([][][][][]i32) =
  let rngs = split_rng 2 <| rng_from_seed seed
  let sizes = getsizes size rngs[0] 5
  let my_arb = (arbitrary5darr arbitraryi32 sizes[0] sizes[1] sizes[2] sizes[3] sizes[4])
  in my_arb size rngs[1]

let prop_show_5d_arr (input : testdata ([][][][][]i32)) =
  match input
  case #testdata arr -> length arr <= 1

let show_show_5d_arr = show_array5d showdecimali32
