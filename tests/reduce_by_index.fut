import "../src/futs/fucheck"
open Fucheck

-- ==
-- input @ red_by_index_100.in
-- output @ red_by_index_100.out
-- input @ red_by_index_1000.in
-- output @ red_by_index_1000.out
-- input @ red_by_index_10000.in
-- output @ red_by_index_10000.out
-- input @ red_by_index_100000.in
-- output @ red_by_index_100000.out
-- input @ red_by_index_1000000.in
-- output @ red_by_index_1000000.out


import "lib/github.com/diku-dk/sorts/radix_sort"

let segscan [n] 't (op: t -> t -> t) (ne: t) (arr: [n](t, bool)) : [n]t =
    let tuples = scan (\ (v1:t, f1:bool) (v2:t, f2:bool) ->
                   ((if f2 then v2 else op v1 v2), (f1 || f2))
                 ) (ne, false) arr
    let (res, _) = unzip tuples
    in res

let segreduce 't [n] (op: t -> t -> t) (ne: t) (arr: [n](t, bool)): []t =
    let scanned = segscan op ne arr
    let (_, flags) = unzip arr
    let (res, _) = unzip <| filter (\(_, f) -> f) <| zip scanned (rotate 1 flags)
    in res

-- added my_ to name and a couple of casts to Ulriks code
-- for use by fucheck
let my_reduce_by_index [m] [n] (dest : *[m]i32)
                    (f : i32 -> i32 -> i32) (ne : i32)
                    (is : [n]i32) (as : [n]i32) : []i32 =
    unsafe
    let (is', is_flags) = unzip <| map2(\iot i ->
                                          if (iot == 0) || is[iot] > is[iot-1]
                                            then (i, true)
                                            else (-1, false)
                                        ) (iota n) is
    let filtered_is_tmp = filter (>=0) is'
    let castsize = length filtered_is_tmp
    let filtered_is = filtered_is_tmp :> [castsize]i32
    let segred_as = segreduce f ne (zip as is_flags) :> [castsize]i32
    let mapme = scatter (replicate m ne) filtered_is segred_as
    in map2 f dest mapme

-- fucheck red_idx
let gen_red_idx (size : i32) (seed : i32) : testdata ([]i32,[]i32,[]i32) =
  let rngs = split_rng 2 <| rng_from_seed seed
  let sizes = getsizes size rngs[0] 2
  let elm_arr_gen = (arbitraryarr arbitraryi32 sizes[0])
  let idx_arr_gen = (arbitraryarr (transformgen i32.abs arbitraryi32) sizes[1])
  in arbitrary3tuple idx_arr_gen elm_arr_gen elm_arr_gen size rngs[1]

let prop_red_idx [m] [n] (input : testdata ([m]i32,[n]i32,[n]i32)) : bool =
  match input
  case #testdata (dest, as, is) ->
    my_reduce_by_index (copy dest) (+) 0 is as == reduce_by_index (copy dest) (+) 0 is as

let show_red_idx [m] [n] (input : testdata ([m]i32,[n]i32,[n]i32)) : []u8 =
  let sn = show_array showdecimali32
  let sm = show_array showdecimali32
  in show3tuple sn sm sm input


let main [m] [n] (dest : [m]i32) (is' : [n]i32) (as' : [n]i32) : []i32 =
  -- I do the sorting here because I couldn't figure out how to do num_bits etc with type 'a
  let (is, as)  = zip is' as' |> radix_sort_by_key (.1) i32.num_bits i32.get_bit |> unzip in
  my_reduce_by_index (copy dest) (+) 0 is as

