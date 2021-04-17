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

import "types"

import "lib/github.com/diku-dk/cpprandom/random"
module r = minstd_rand
module dist = uniform_int_distribution i64 r

module Rng = {
  open Types

  type rng = r.rng

  let snd (_,b) = b
  let fsnd f (x, y) = (x, f y)

  entry rng_from_seed seed = r.rng_from_seed [seed]
  let split_rng n rng = r.split_rng n rng

  let rand_i64 ((low,high) : (i64,i64)) (rng : rng) : (rng, i64) =
    dist.rand (low, high) rng

  let rand_i32 ((low,high) : (i32,i32)) (rng : rng) : (rng, i32) =
    fsnd i32.i64 <| dist.rand (i64.i32 low, i64.i32 high) rng

  let rand_i16 ((low,high) : (i16,i16)) (rng : rng) : (rng, i16) =
    fsnd i16.i64 <| dist.rand (i64.i16 low, i64.i16 high) rng

  let rand_i8 ((low,high) : (i8,i8)) (rng : rng) : (rng, i8) =
    fsnd i8.i64 <| dist.rand (i64.i8 low, i64.i8 high) rng

  let rand_u64 ((low,high) : (u64,u64)) (rng : rng) : (rng, u64) =
    let negate_high_bit bs = 0x8000000000000000 ^ bs
    in fsnd (negate_high_bit <-< u64.i64)
       <| dist.rand ( i64.u64 (negate_high_bit low)
                    , i64.u64 (negate_high_bit high)
                    ) rng

  let rand_u32 ((low,high) : (u32,u32)) (rng : rng) : (rng, u32) =
    fsnd u32.i64 <| dist.rand (i64.u32 low, i64.u32 high) rng

  let rand_u16 ((low,high) : (u16,u16)) (rng : rng) : (rng, u16) =
    fsnd u16.i64 <| dist.rand (i64.u16 low, i64.u16 high) rng

  let rand_u8 ((low,high) : (u8,u8)) (rng : rng) : (rng, u8) =
    fsnd u8.i64 <| dist.rand (i64.u8 low, i64.u8 high) rng

  let rand_bool ((low,high) : (bool,bool)) (rng : rng) : (rng, bool) =
    fsnd bool.i64 <| dist.rand (i64.bool low, i64.bool high) rng

  let getsizes (maxsize : size) (rng : rng) (num : i64) : (rng, [num]i64) =
    let rngs =
      r.split_rng num rng
    let (rngs, sizes) =
      unzip (map (\rng -> rand_i64 (0,maxsize) rng) rngs)
    in (r.join_rng rngs, sizes)

}
