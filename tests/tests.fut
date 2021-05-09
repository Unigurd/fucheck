-- Copyright (C) Sigurd Dam Sonniks

-- This file is part of Fucheck.

--     Fucheck is free software: you can redistribute it and/or modify
--     it under the terms of the GNU General Public License as published by
--     the Free Software Foundation, either version 3 of the License, or
--     (at your option) any later version.

--     Fucheck is distributed in the hope that it will be useful,
--     but WITHOUT ANY WARRANTY; without even the implied warranty of
--     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--     GNU General Public License for more details.

--     You should have received a copy of the GNU General Public License
--     along with Fucheck.  If not, see <https://www.gnu.org/licenses/>.

import "lib/github.com/diku-dk/cpprandom/random"
module r = minstd_rand
import "../src/futs//fucheck"
open Fucheck

entry rng_from_seed seed = r.rng_from_seed [seed]

type maybe 'a = #just a | #nothing
let maybegen 't elmgen : gen (maybe t) =
  oneof2 (transformgen (\i -> #just i) elmgen)
         (constgen #nothing)

let show_maybe 't (elmshow : t -> []u8) (input : maybe t) : []u8 =
  match input
  case (#just i) -> "#just " ++ elmshow i
  case #nothing -> "#nothing"

-- fucheck pass
entry gen_pass = arbitraryi32

entry prop_pass (i : i32) : bool = i == i

entry show_pass = showdecimali32

let labels_pass (i : i32) =
   if i < 7 then "i < 7" else "i >= 7"

entry state_pass : state = { maxtests = 305 , maxsize = 513, maxdiscardedratio = 100 }

-- fucheck failWithShow
entry gen_failWithShow = arbitraryi32

entry prop_failWithShow (i : i32) = i != i

entry show_failWithShow = showdecimali32

-- fucheck failWithoutShow
entry gen_failWithoutShow = arbitraryi32

entry prop_failWithoutShow (i : i32) = i != i

-- fucheck tupleMightFail
entry gen_tupleMightFail = arbitrarytuple arbitraryi32 arbitraryi32

entry prop_tupleMightFail ((i,j) : (i32,i32)) = i == j


entry show_tupleMightFail = show2tuple showdecimali32 showdecimali32

-- fucheck bool
entry gen_bool = arbitrarybool

entry prop_bool b = b : bool

entry show_bool = showbool

entry state_bool : state = { maxtests = 55 , maxsize = 101, maxdiscardedratio = 100 }

-- fucheck cond

entry state_cond :{maxtests: i32, maxsize: i64, maxdiscardedratio: i32} =
  { maxtests = 157 , maxsize = 131, maxdiscardedratio = 100 }

entry gen_cond = arbitrarytuple arbitraryi32 arbitraryi32

entry cond_cond ((i,j) : (i32, i32)) = i <= j

entry prop_cond ((i,j) : (i32, i32)) = i <= j

entry show_cond = show2tuple showdecimali32 showdecimali32

entry labels_cond ((i,j): (i32,i32)) : []u8 =
  "Difference of at least " ++ (showdecimali32 ((j - i) / 10 * 10))

-- fucheck zip 1
let gen_zip arrsize : gen ([arrsize]i32, [arrsize]i32) = \size rng ->
  let my_arb = (arbitraryarr arbitraryi32 arrsize)
  in arbitrarytuple my_arb my_arb size rng

entry prop_zip [n] ((as,bs) : ([n]i32,[n]i32)) : bool =
   (as,bs) == unzip (zip as bs)

-- fucheck transpose 2
entry gen_transpose arrsize0 arrsize1 : gen ([arrsize0][arrsize1]i32) = \size rng ->
  scale (10+) (arbitrary2darr arbitraryi32 arrsize0 arrsize1) size rng

entry prop_transpose [n] [m] (matrix : [n][m]i32) : bool =
  matrix == transpose (transpose matrix)

entry show_transpose = show_array2d showdecimali32

-- fucheck badzip 1
entry gen_badzip arrsize =
  arbitraryarr (arbitrarytuple arbitraryi32 arbitraryi32) arrsize

entry prop_badzip [n] (arr : [n](i32,i32)) : bool =
  arr != uncurry zip (unzip arr)

entry show_badzip = show_array (show2tuple showdecimali32 showdecimali32)

-- fucheck failsize 1
entry gen_failsize arrsize =
  arbitraryarr arbitraryi32 arrsize

entry prop_failsize [n] (arr : [n]i32) : bool =
  length arr < 10

entry show_failsize = show_array showdecimali32

-- fucheck justfail
entry gen_justfail = maybegen arbitraryi32

entry prop_justfail (input : maybe i32) : bool =
  match input
  case #just i-> false
  case #nothing -> true

entry show_justfail = show_maybe showdecimali32

-- fucheck nothingfail
entry gen_nothingfail = maybegen arbitraryi32

entry prop_nothingfail (input : (maybe i32)) : bool =
  !(prop_justfail input)

entry show_nothingfail = show_maybe showdecimali32

-- fucheck maybearr 1
entry gen_maybearr arrsize =
  arbitraryarr (maybegen arbitraryi32) arrsize

entry prop_maybearr [n] (arr : [n](maybe i32)) : bool =
  length arr < 10

entry show_maybearr = show_array (show_maybe showdecimali32)

-- fucheck show_crash
let gen_show_crash = arbitraryi32
let prop_show_crash (i : i32) = false
let show_show_crash (input : i32) =
  match input
  case i ->
    let c = i/0
    in showdecimali32 c

-- fucheck rev_id 1
let gen_rev_id arrsize =
  arbitraryarr arbitraryi32 arrsize

let prop_rev_id (arr : []i32) =
  reverse (reverse arr) == arr

-- fucheck show_2d_arr 2
let gen_show_2d_arr = arbitrary2darr arbitraryi32

let prop_show_2d_arr (arr : [][]i32) =
  length arr <= 2

let show_show_2d_arr = show_array2d showdecimali32

-- fucheck show_3d_arr 3
let gen_show_3d_arr = arbitrary3darr arbitraryi32

let prop_show_3d_arr (arr : [][][]i32) =
  length arr <= 2

let show_show_3d_arr = show_array3d showdecimali32

-- fucheck show_4d_arr 4
let gen_show_4d_arr = arbitrary4darr arbitraryi32

let prop_show_4d_arr (arr : [][][][]i32) =
  length arr <= 2

let show_show_4d_arr = show_array4d showdecimali32

-- fucheck show_5d_arr 5
let gen_show_5d_arr = arbitrary5darr arbitraryi32
--let gen_show_5d_arr (size : size) (seed : i32) : testdata ([][][][][]i32) =
--  let rngs = split_rng 2 <| rng_from_seed seed
--  let sizes = getsizes size rngs[0] 5
--  let my_arb = arbitrary5darr arbitraryi32 sizes[0] sizes[1] sizes[2] sizes[3] sizes[4]
--  in my_arb size rngs[1]

let prop_show_5d_arr (arr : [][][][][]i32) =
  length arr <= 1

let show_show_5d_arr = show_array5d showdecimali32
