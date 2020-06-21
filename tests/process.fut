import "../src/futs/fucheck"
open Fucheck

let process (s1: []i32) (s2: []i32): i32 =
  reduce i32.max 0 (map i32.abs (map2 (-) s1 s2))

let process_gen : gen ([](i32, i32)) = \size rng ->
  let (rng, sizes) = getsizes size rng 1
  in arbitraryarr (arbitrarytuple arbitraryi32 arbitraryi32) sizes[0] size rng

let process_show [n] (input : [n](i32, i32)) : []u8 =
  showtuple (show_array showdecimali32) (show_array showdecimali32) (unzip input)

let main (s1: []i32) (s2: []i32) : i32 =
  process s1 s2

-- testing with fucheck begins here

-- fucheck proc_comm
let gen_proc_comm = process_gen
let prop_proc_comm [n] (arr : ([n](i32, i32))) : bool =
  let (arr0,arr1) = unzip arr
  in process arr0 arr1 == process arr1 arr0

-- fucheck proc_neg
let gen_proc_neg = process_gen
let prop_proc_neg [n] (arr : [n](i32, i32)) : bool =
  let (arr0,arr1) = unzip arr
  in process (map (0-) arr0) arr1 == process arr1 (map (0-) arr0)

-- fucheck proc_maxdiff
let gen_proc_maxdiff = process_gen
let prop_proc_maxdiff [n] (arr : [n](i32, i32)) : bool =
  let (arr0,arr1) = unzip arr
  let mx = i32.max (reduce i32.max i32.lowest arr0) (reduce i32.max i32.lowest arr1)
  let mn = i32.min (reduce i32.min i32.highest arr0) (reduce i32.min i32.highest arr1)
  let m_diff = mx - mn
  let processed = process arr0 arr1
  in m_diff >= processed

let show_proc_maxdiff = process_show

