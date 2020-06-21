import "../src/futs/fucheck"
import "process"
open Fucheck

let maxTup (x1:i32, i1:i32) (x2:i32, i2:i32) : (i32, i32) =
  if x1 > x2 then (x1, i1) else
  if x2 > x1 then (x2, i2) else
  if i1 > i2 then (x1, i1) else (x2, i2)

let process_idx [n] (s1: [n]i32) (s2: [n]i32): (i32, i32) =
  reduce_comm maxTup (0,-1) (zip (map i32.abs (map2 (-) s1 s2)) (iota n))

-- testing with fucheck begins here

let fst (a,_) = a

-- fucheck proc
let gen_proc = process_gen
let prop_proc [n] ((arr0,arr1) : ([n]i32, [n]i32)) : bool =
  process arr0 arr1 == fst (process_idx arr0 arr1)

let show_proc = process_show

-- fucheck idx
let gen_idx = process_gen
let prop_idx [n] ((arr0,arr1) : ([n]i32, [n]i32)) : bool =
  let (diff,idx) = process_idx arr0 arr1
  let idx_diff = if n == 0 then 0 else i32.abs (arr0[idx] - arr1[idx])
  in diff == idx_diff

let show_idx = process_show
