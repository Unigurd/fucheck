import "../src/futs /fucheck"
open Fucheck

let distrib_state : state = { maxtests = 10000 , maxsize = 100, maxdiscardedratio = 100}
--let distrib_gen 't (gen : gen t) (size : size) (seed : i32) : testdata t =
--  resize 9 gen size <| rng_from_seed seed

-- fucheck bool
let gen_bool (_ : i32) (seed : i32) = arbitrarybool seed <| rng_from_seed seed

let prop_bool (_ : testdata bool) = true

let labels_bool (input : testdata bool) = showbool input

let state_bool = distrib_state

-- fucheck i8
let gen_i8 (size : i32) (seed : i32) = resize 12 arbitraryi8 size <| rng_from_seed seed

let prop_i8 (_ : testdata i8) = true

let labels_i8 (input : testdata i8) =  showdecimali32 <| testdata <| i32.i8 <| get input

let state_i8 = distrib_state

-- fucheck i16
let gen_i16 (size : i32) (seed : i32) = resize 12 arbitraryi16 size <| rng_from_seed seed

let prop_i16 (_ : testdata i16) = true

let labels_i16 (input : testdata i16) =  showdecimali32 <| testdata <| i32.i16 <| get input

let state_i16 = distrib_state

-- fucheck i32
let gen_i32 (size : i32) (seed : i32) = resize 12 arbitraryi32 size <| rng_from_seed seed

let prop_i32 (_ : testdata i32) = true

let labels_i32 (input : testdata i32) = showdecimali32 input

let state_i32 = distrib_state

-- fucheck i64
let gen_i64 (size : i32) (seed : i32) = resize 12 arbitraryi64 size <| rng_from_seed seed

let prop_i64 (_ : testdata i64) = true

let labels_i64 (input : testdata i64) =  showdecimali32 <| testdata <| i32.i64 <| get input

let state_i64 = distrib_state

-- fucheck u8
let gen_u8 (size : i32) (seed : i32) = resize 9 arbitraryu8 size <| rng_from_seed seed

let prop_u8 (_ : testdata u8) = true

let labels_u8 (input : testdata u8) =  showdecimali32 <| testdata <| i32.u8 <| get input

let state_u8 = distrib_state

-- fucheck u16
let gen_u16 (size : i32) (seed : i32) = resize 9 arbitraryu16 size <| rng_from_seed seed

let prop_u16 (_ : testdata u16) = true

let labels_u16 (input : testdata u16) =  showdecimali32 <| testdata <| i32.u16 <| get input

let state_u16 = distrib_state

-- fucheck u32
let gen_u32 (size : i32) (seed : i32) = resize 9 arbitraryu32 size <| rng_from_seed seed

let prop_u32 (_ : testdata u32) = true

let labels_u32 (input : testdata u32) =  showdecimali32 <| testdata <| i32.u32 <| get input

let state_u32 = distrib_state

-- fucheck u64
let gen_u64 (size : i32) (seed : i32) = resize 9 arbitraryu64 size <| rng_from_seed seed

let prop_u64 (_ : testdata u64) = true

let labels_u64 (input : testdata u64) =  showdecimali32 <| testdata <| i32.u64 <| get input

let state_u64 = distrib_state

-- fucheck arr
let gen_arr (_ : size) (seed : i32) =
   let rngs = split_rng 2 <| rng_from_seed seed
   in arbitraryarr arbitraryi32 2 2 rngs[1]

let prop_arr (_ : testdata ([]i32)) = true

let labels_arr (input : testdata ([]i32)) = show_array showdecimali32 input

let state_arr = distrib_state

-- fucheck arr2d
let gen_arr2d (_ : size) (seed : i32) : testdata ([][]bool) =
   let rngs = split_rng 2 <| rng_from_seed seed
   in arbitrary2darr arbitrarybool 4 2 2 rngs[1]

let prop_arr2d (_ : testdata ([][]bool)) = true

let labels_arr2d (input : testdata ([][]bool)) = show_array2d showbool input

let state_arr2d = distrib_state

-- fucheck sizes
let gen_sizes (_ : size) (seed : i32) : testdata ([]i32) =
   let rngs = split_rng 2 <| rng_from_seed seed
   let sizes = getsizes 4 rngs[0] 2
   in testdata sizes

let prop_sizes (_ : testdata ([]i32)) = true

let labels_sizes (input : testdata ([]i32)) = show_array showdecimali32 input

let state_sizes = distrib_state

 -- fucheck const
let gen_const (size : i32) (seed : i32) : testdata i32 = constgen 0 size <| rng_from_seed seed

let prop_const (_ : testdata i32) : bool = true

let labels_const (input : testdata i32) : []u8 = showdecimali32 input

let state_const = distrib_state

-- fucheck freq2
let gen_freq2 (size : i32) (seed : i32) : testdata i32 =
  frequencyof2 (2,(constgen 40))
               (3,(constgen 60))
               size <| rng_from_seed seed

let prop_freq2 (_ : testdata i32) = true

let labels_freq2 (input : testdata i32) = showdecimali32 input

let state_freq2 = distrib_state

-- fucheck freq3
let gen_freq3 (size : i32) (seed : i32) : testdata i32 =
  frequencyof3 (2,(constgen 20))
               (3,(constgen 30))
               (5,(constgen 50))
               size <| rng_from_seed seed

let prop_freq3 (_ : testdata i32) = true

let labels_freq3 (input : testdata i32) = showdecimali32 input

let state_freq3 = distrib_state

-- fucheck freq4
let gen_freq4 (size : i32) (seed : i32) : testdata i32 =
  frequencyof4 (1,(constgen 10))
               (2,(constgen 20))
               (3,(constgen 30))
               (4,(constgen 40))
               size <| rng_from_seed seed

let prop_freq4 (_ : testdata i32) = true

let labels_freq4 (input : testdata i32) = showdecimali32 input

let state_freq4 = distrib_state

-- fucheck freq5
let gen_freq5 (size : i32) (seed : i32) : testdata i32 =
  frequencyof5 (2,(constgen 10))
               (3,(constgen 15))
               (4,(constgen 20))
               (5,(constgen 25))
               (6,(constgen 30))
               size <| rng_from_seed seed

let prop_freq5 (_ : testdata i32) = true

let labels_freq5 (input : testdata i32) = showdecimali32 input

let state_freq5 = distrib_state

-- fucheck oneof2
let gen_oneof2 (size : i32) (seed : i32) : testdata i32 =
  oneof2 (constgen 0)
         (constgen 1)
         size <| rng_from_seed seed

let prop_oneof2 (_ : testdata i32) = true

let labels_oneof2 (input : testdata i32) = showdecimali32 input

let state_oneof2 = distrib_state

-- fucheck oneof3
let gen_oneof3 (size : i32) (seed : i32) : testdata i32 =
  oneof3 (constgen 0)
         (constgen 1)
         (constgen 2)
         size <| rng_from_seed seed

let prop_oneof3 (_ : testdata i32) = true

let labels_oneof3 (input : testdata i32) = showdecimali32 input

let state_oneof3 = distrib_state

-- fucheck oneof4
let gen_oneof4 (size : i32) (seed : i32) : testdata i32 =
  oneof4 (constgen 0)
         (constgen 1)
         (constgen 2)
         (constgen 3)
         size <| rng_from_seed seed

let prop_oneof4 (_ : testdata i32) = true

let labels_oneof4 (input : testdata i32) = showdecimali32 input

let state_oneof4 = distrib_state

-- fucheck oneof5
let gen_oneof5 (size : i32) (seed : i32) : testdata i32 =
  oneof5 (constgen 0)
         (constgen 1)
         (constgen 2)
         (constgen 3)
         (constgen 4)
         size <| rng_from_seed seed

let prop_oneof5 (_ : testdata i32) = true

let labels_oneof5 (input : testdata i32) = showdecimali32 input

let state_oneof5 = distrib_state

-- fucheck elements
let gen_elements (size : i32) (seed : i32) : testdata i32 =
  elements [0,1,2,3,4,5,6,7,8,9] size <| rng_from_seed seed

let prop_elements (_ : testdata i32) = true

let labels_elements (input : testdata i32) = showdecimali32 input

let state_elements = distrib_state

-- fucheck tup2
let gen_tup2 (_ : i32) (seed : i32) : testdata (i32,i32) =
  arbitrarytuple arbitraryi32 arbitraryi32 2 <| rng_from_seed seed

let prop_tup2 (_ : testdata (i32,i32)) = true

let labels_tup2 (input : testdata (i32,i32)) = show2tuple showdecimali32 showdecimali32 input

let state_tup2 = distrib_state

-- fucheck tup3
let gen_tup3 (_ : i32) (seed : i32) : testdata (i32,bool,bool) =
  resize 2 (arbitrary3tuple arbitraryi32 arbitrarybool arbitrarybool) 2 <| rng_from_seed seed

let prop_tup3 (_ : testdata (i32,bool,bool)) = true

let labels_tup3 (input : testdata (i32,bool,bool)) =
  show3tuple showdecimali32 showbool showbool input

let state_tup3 = distrib_state

-- fucheck tup4
let gen_tup4 (_ : i32) (seed : i32) : testdata (i32,i32,bool,bool) =
  resize 2 (arbitrary4tuple arbitraryi32 arbitraryi32 arbitrarybool arbitrarybool) 2 <| rng_from_seed seed

let prop_tup4 (_ : testdata (i32,i32,bool,bool)) = true

let labels_tup4 (input : testdata (i32,i32,bool,bool)) =
  show4tuple showdecimali32 showdecimali32 showbool showbool input

let state_tup4 = distrib_state
