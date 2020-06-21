-- Copyright (C) Sigurd Dam Sonniks

-- This file is part of Fucheck.

-- Fucheck is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- Fucheck is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with Fucheck.  If not, see <https://www.gnu.org/licenses/>.

import "../src/futs/fucheck"
import "lib/github.com/diku-dk/sorts/radix_sort"
open Fucheck

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

-- Changes introduced for fucheck testing:
-- added my_ to name and a couple of casts since the code
-- was written before static size types were added
let my_reduce_by_index [m] [n] (dest : *[m]i32)
                    (f : i32 -> i32 -> i32) (ne : i32)
                    (is : [n]i32) (as : [n]i32) : []i32 =
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

let main [m] [n] (dest : [m]i32) (is' : [n]i32) (as' : [n]i32) : []i32 =
  -- I do the sorting here because I couldn't figure out how to do num_bits etc with type 'a
  let (is, as)  = zip is' as' |> radix_sort_by_key (.1) i32.num_bits i32.get_bit |> unzip in
  my_reduce_by_index (copy dest) (+) 0 is as

-- testing with fucheck begins here

-- fucheck red_idx
let gen_red_idx : gen ([]i32,[]i32,[]i32) = \size rng ->
  let (rng, sizes) = getsizes size rng 2
  let elm_arr_gen = arbitraryarr arbitraryi32 sizes[0]
  let idx_arr_gen = arbitraryarr (transformgen i32.abs arbitraryi32) sizes[1]
  in arbitrary3tuple idx_arr_gen elm_arr_gen elm_arr_gen size rng

let prop_red_idx [m] [n] ((dest, as, is) : ([m]i32,[n]i32,[n]i32)) : bool =
  my_reduce_by_index (copy dest) (+) 0 is as == reduce_by_index (copy dest) (+) 0 is as

let show_red_idx [m] [n] (input : ([m]i32,[n]i32,[n]i32)) : []u8 =
  let sn = show_array showdecimali32
  let sm = show_array showdecimali32
  in show3tuple sn sm sm input


