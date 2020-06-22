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
open Fucheck

let distrib_state : state = { maxtests = 10000 , maxsize = 100, maxdiscardedratio = 100}
--let distrib_gen 't (gen : gen t) (size : size) (seed : i32) : testdata t =
--  resize 9 gen size <| rng_from_seed seed

-- fucheck bool
let gen_bool : gen bool = arbitrarybool

let prop_bool (_ : bool) = true

let labels_bool = showbool

let state_bool = distrib_state

-- fucheck i8
let gen_i8 : gen i8 = resize 12 arbitraryi8 

let prop_i8 (_ : i8) = true

let labels_i8 = showi8

let state_i8 = distrib_state

-- fucheck i16
let gen_i16 : gen i16 = resize 12 arbitraryi16

let prop_i16 (_ : i16) = true

let labels_i16 = showi16

let state_i16 = distrib_state

-- fucheck i32
let gen_i32 : gen i32 = resize 12 arbitraryi32

let prop_i32 (_ : i32) = true

let labels_i32 = showdecimali32

let state_i32 = distrib_state

-- holup
-- fucheck i64
let gen_i64 : gen i64 = resize 12 arbitraryi64

let prop_i64 (_ : i64) = true

let labels_i64 = showdecimali64

let state_i64 = distrib_state

-- fucheck u8
let gen_u8 : gen u8 = resize 9 arbitraryu8

let prop_u8 (_ : u8) = true

let labels_u8 = showdecimalu8

let state_u8 = distrib_state

-- fucheck u16
let gen_u16 : gen u16 = resize 9 arbitraryu16

let prop_u16 (_ : u16) = true

let labels_u16 = showdecimalu16

let state_u16 = distrib_state

-- fucheck u32
let gen_u32 : gen u32 = resize 9 arbitraryu32

let prop_u32 (_ : u32) = true

let labels_u32 = showdecimalu32

let state_u32 = distrib_state

-- holup
-- fucheck u64
let gen_u64 : gen u64 = resize 9 arbitraryu64

let prop_u64 (_ : u64) = true

let labels_u64 = showdecimalu64

let state_u64 = distrib_state

-- fucheck arr
let gen_arr _ rng : (rng,[2]i32) =
   arbitraryarr arbitraryi32 2 2 rng

let prop_arr (_ : [2]i32) = true

let labels_arr = show_array showdecimali32

let state_arr = distrib_state

-- fucheck arr2d
let gen_arr2d _ rng : (rng,([2][2]bool)) =
  arbitrary2darr arbitrarybool 2 2 2 rng

let prop_arr2d (_ : ([2][2]bool)) = true

let labels_arr2d = show_array2d showbool

let state_arr2d = distrib_state

--  fucheck const
let gen_const : gen i32 = constgen 0

let prop_const (_ : i32) : bool = true

let labels_const = showdecimali32

let state_const = distrib_state

-- fucheck freq2
let gen_freq2 : gen i32 =
  frequencyof2 (2,(constgen 40))
               (3,(constgen 60))

let prop_freq2 (_ : i32) = true

let labels_freq2 = showdecimali32

let state_freq2 = distrib_state

-- fucheck freq3
let gen_freq3 : gen i32 =
  frequencyof3 (2,(constgen 20))
               (3,(constgen 30))
               (5,(constgen 50))

let prop_freq3 (_ : i32) = true

let labels_freq3  = showdecimali32

let state_freq3 = distrib_state

-- fucheck freq4
let gen_freq4 : gen i32 =
  frequencyof4 (1,(constgen 10))
               (2,(constgen 20))
               (3,(constgen 30))
               (4,(constgen 40))

let prop_freq4 (_ : i32) = true

let labels_freq4 = showdecimali32

let state_freq4 = distrib_state

-- fucheck freq5
let gen_freq5 : gen i32 =
  frequencyof5 (2,(constgen 10))
               (3,(constgen 15))
               (4,(constgen 20))
               (5,(constgen 25))
               (6,(constgen 30))

let prop_freq5 (_ : i32) = true

let labels_freq5 = showdecimali32

let state_freq5 = distrib_state

-- fucheck oneof2
let gen_oneof2 : gen i32 =
  oneof2 (constgen 0)
         (constgen 1)

let prop_oneof2 (_ : i32) = true

let labels_oneof2 = showdecimali32

let state_oneof2 = distrib_state

-- fucheck oneof3
let gen_oneof3 : gen i32 =
  oneof3 (constgen 0)
         (constgen 1)
         (constgen 2)

let prop_oneof3 (_ : i32) = true

let labels_oneof3 = showdecimali32

let state_oneof3 = distrib_state

-- fucheck oneof4
let gen_oneof4 : gen i32 =
  oneof4 (constgen 0)
         (constgen 1)
         (constgen 2)
         (constgen 3)

let prop_oneof4 (_ : i32) = true

let labels_oneof4 = showdecimali32

let state_oneof4 = distrib_state

-- fucheck oneof5
let gen_oneof5 : gen i32 =
  oneof5 (constgen 0)
         (constgen 1)
         (constgen 2)
         (constgen 3)
         (constgen 4)

let prop_oneof5 (_ : i32) = true

let labels_oneof5 = showdecimali32

let state_oneof5 = distrib_state

-- fucheck elements
let gen_elements : gen i32 =
  elements [0,1,2,3,4,5,6,7,8,9]

let prop_elements (_ :  i32) = true

let labels_elements = showdecimali32

let state_elements = distrib_state

-- fucheck tup2
let gen_tup2 : gen (i32,i32) =
  resize 2 <| arbitrarytuple arbitraryi32 arbitraryi32

let prop_tup2 (_ : (i32,i32)) = true

let labels_tup2 = show2tuple showdecimali32 showdecimali32

let state_tup2 = distrib_state

-- fucheck tup3
let gen_tup3 : gen (i32,bool,bool) =
  resize 2 (arbitrary3tuple arbitraryi32 arbitrarybool arbitrarybool)

let prop_tup3 (_ : (i32,bool,bool)) = true

let labels_tup3 =
  show3tuple showdecimali32 showbool showbool

let state_tup3 = distrib_state

-- fucheck tup4
let gen_tup4 : gen (i32,i32,bool,bool) =
  resize 2 (arbitrary4tuple arbitraryi32 arbitraryi32 arbitrarybool arbitrarybool)

let prop_tup4 (_ : (i32,i32,bool,bool)) = true

let labels_tup4 =
  show4tuple showdecimali32 showdecimali32 showbool showbool

let state_tup4 = distrib_state
