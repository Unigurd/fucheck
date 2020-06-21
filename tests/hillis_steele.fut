import "../src/futs/fucheck"
open Fucheck

let ilog2 (x: i32) = 31 - i32.clz x

let hillis_steele [n] (xs: [n]i32) : [n]i32 =
  unsafe
  let m = ilog2 n in
  loop xs = copy xs for d in (0...m) do
    let ofs = 2 ** d in
    map3 (\i1 xr x -> if i1 < ofs then x else x + xr) (iota n) (rotate (-ofs) xs) xs

let main (s: []i32) : []i32 =
  hillis_steele s

-- testing with fucheck begins here

-- fucheck hillis
let gen_hillis (size : i32) (seed : i32) : testdata ([]i32) =
  let rngs = split_rng 2 <| rng_from_seed seed
  let sizes = getsizes size rngs[0] 1
  let arrgen = arbitraryarr arbitraryi32 <| i32.max 0 <| ilog2 sizes[0]
  in arrgen size rngs[1]

let prop_hillis (input : testdata ([]i32)) : bool =
  match input
  case #testdata arr ->
    scan (+) 0 arr == hillis_steele arr

let show_hillis (input : testdata ([]i32)) : []u8 =
  show_array showdecimali32 input

