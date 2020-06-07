import "../src/futs/fucheck"
open Fucheck

-- ==
-- input @ two_100_i32s
-- input @ two_1000_i32s
-- input @ two_10000_i32s
-- input @ two_100000_i32s
-- input @ two_1000000_i32s
-- input @ two_5000000_i32s
-- input @ two_10000000_i32s

let process (s1: []i32) (s2: []i32): i32 =
  reduce i32.max 0 (map i32.abs (map2 (-) s1 s2))

let process_gen (size : size) (seed : i32) : testdata ([]i32, []i32) =
  let rngs = split_rng 2 <| rng_from_seed seed
  let sizes = getsizes size rngs[0] 1
  let arrgen = (arbitraryarr arbitraryi32 sizes[0])
  in arbitrarytuple arrgen arrgen size rngs[1]

let process_show [n] (input : testdata ([n]i32, [n]i32)) : []u8 =
    showtuple (show_array showdecimali32) (show_array showdecimali32) input

-- fucheck proc_comm
let gen_proc_comm = process_gen
let prop_proc_comm [n] (input : testdata ([n]i32, [n]i32)) : bool =
  match input
  case #testdata (arr0,arr1) -> process arr0 arr1 == process arr1 arr0

-- fucheck proc_neg
let gen_proc_neg = process_gen
let prop_proc_neg [n] (input : testdata ([n]i32, [n]i32)) : bool =
  match input
  case #testdata (arr0,arr1) -> process (map (0-) arr0) arr1 == process arr1 (map (0-) arr0)

-- fucheck proc_maxdiff
let gen_proc_maxdiff = process_gen
let prop_proc_maxdiff [n] (input : testdata ([n]i32, [n]i32)) : bool =
  match input
  case #testdata (arr0,arr1) ->
    let mx = i32.max (reduce i32.max i32.lowest arr0) (reduce i32.max i32.lowest arr1)
    let mn = i32.min (reduce i32.min i32.highest arr0) (reduce i32.min i32.highest arr1)
    let m_diff = mx - mn
    let processed = process arr0 arr1
    in m_diff >= processed

let show_proc_maxdiff = process_show

let main (s1: []i32) (s2: []i32) : i32 =
  process s1 s2
