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

import "rng"

module Gen = {
  open Rng

  type^ gen 'a = size -> rng -> (rng, a)

  -- instead of arbitrarysortedarray
  -- so as not to write a sorting function / add a dependency
  -- simply supply an array generator and a sorting function
  let transformgen 'a 'b (f : a -> b) (g : gen a) : gen b =
    (\size rng -> let (rng, x) = g size rng in (rng, f x))

  let choose_i8 (bounds : (i8,i8)) : gen i8 =
    (\_ r ->  rand_i8 bounds r)

  let choose_i16 (bounds : (i16,i16)) : gen i16 =
    (\_ r -> rand_i16 bounds r)

  let choose_i32 (bounds : (i32,i32)) : gen i32 =
    (\_ r -> rand_i32 bounds r)

  let choose_i64 (bounds : (i64,i64)) : gen i64 =
    (\_ r -> rand_i64 bounds r)

  let choose_u8 (bounds : (u8,u8)) : gen u8 =
    (\_ r -> rand_u8 bounds r)

  let choose_u16 (bounds : (u16,u16)) : gen u16 =
    (\_ r -> rand_u16 bounds r)

  let choose_u32 (bounds : (u32,u32)) : gen u32 =
    (\_ r -> rand_u32 bounds r)

  let choose_u64 (bounds : (u64,u64)) : gen u64 =
    (\_ r -> rand_u64 bounds r)

  let choose_bool (bounds : (bool,bool)) : gen bool =
    (\_ r -> rand_bool bounds r)

  let sized 'a (fgen : size -> gen a) : gen a =
    (\n r ->
       match (fgen n)
       case  m -> m n r)

  let resize 'elm (newsize : size) (oldgen : gen elm) : gen elm =
    (\_ rng -> oldgen newsize rng)

  let scale 'elm (fun : i64 -> i64) (oldgen : gen elm) : gen elm =
    (\size rng -> oldgen (fun size) rng)

  let constgen 't (const : t) : gen t =
    (\_ rng -> (rng, const))

  let frequencyof2 'elm
                 ((freq0,gen0) : (i32, gen elm))
                 ((freq1,gen1) : (i32, gen elm))
                 : gen elm =
    (\size rng ->
       let totalfreq = freq0 + freq1
       let (rng, f) = rand_i32 (1,totalfreq) rng
       in if f <= freq0
          then gen0 size rng
          else gen1 size rng)

  let oneof2 'elm
           (gen0 : gen elm)
           (gen1 : gen elm)
           : gen elm =
    frequencyof2 (1,gen0) (1,gen1)


  let frequencyof3 'elm
                 ((freq0,gen0) : (i32, gen elm))
                 ((freq1,gen1) : (i32, gen elm))
                 ((freq2,gen2) : (i32, gen elm))
                 : gen elm =
    frequencyof2 (freq0 + freq1, frequencyof2 (freq0,gen0) (freq1,gen1))
                 (freq2,gen2)

  let oneof3 'elm
           (gen0 : gen elm)
           (gen1 : gen elm)
           (gen2 : gen elm)
           : gen elm =
    frequencyof3 (1,gen0) (1,gen1) (1,gen2)

  let frequencyof4 'elm
                 ((freq0,gen0) : (i32, gen elm))
                 ((freq1,gen1) : (i32, gen elm))
                 ((freq2,gen2) : (i32, gen elm))
                 ((freq3,gen3) : (i32, gen elm))
                 : gen elm =
    frequencyof2 (freq0 + freq1, frequencyof2 (freq0,gen0) (freq1,gen1))
                 (freq2 + freq3, frequencyof2 (freq2,gen2) (freq3,gen3))

  let oneof4 'elm
           (gen0 : gen elm)
           (gen1 : gen elm)
           (gen2 : gen elm)
           (gen3 : gen elm)
           : gen elm =
    frequencyof4 (1,gen0) (1,gen1) (1,gen2) (1,gen3)

  let frequencyof5 'elm
                 ((freq0,gen0) : (i32, gen elm))
                 ((freq1,gen1) : (i32, gen elm))
                 ((freq2,gen2) : (i32, gen elm))
                 ((freq3,gen3) : (i32, gen elm))
                 ((freq4,gen4) : (i32, gen elm))
                 : gen elm =
    frequencyof2 (freq0 + freq1,         frequencyof2 (freq0,gen0) (freq1,gen1))
                 (freq2 + freq3 + freq4, frequencyof3 (freq2,gen2) (freq3,gen3) (freq4,gen4))

  let oneof5 'elm
           (gen0 : gen elm)
           (gen1 : gen elm)
           (gen2 : gen elm)
           (gen3 : gen elm)
           (gen4 : gen elm)
           : gen elm =
    frequencyof5 (1,gen0) (1,gen1) (1,gen2) (1,gen3) (1,gen4)


  let elements 'elm [n] (elms : [n]elm) : gen elm =
    (\_ rng ->
       let (rng, i) = rand_i64 (0,n-1) rng
       in (rng, elms[i]))


  let arbitrarybool : gen bool =  choose_bool (false,true)

  let arbitraryi8   : gen i8   =
    sized (\n ->
             let n' = i8.i64 n
             in choose_i8 (if n <= i64.i8 i8.highest
                           then (-n',n')
                           else (i8.lowest, i8.highest)))

  let arbitraryi16   : gen i16   =
    sized (\n ->
             let n' = i16.i64 n
             in choose_i16 (if n <= i64.i16 i16.highest
                            then (-n',n')
                            else (i16.lowest, i16.highest)))

  let arbitraryi32  : gen i32  =
    sized (\n ->
             let n' = i32.i64 n
             in choose_i32 (if n <= i64.i32 i32.highest
                            then (-n',n')
                            else (i32.lowest, i32.highest)))

    -- sized (\n -> choose_i32 (if n < i32.highest
    --                          then (-n,n)
    --                          else (i32.lowest, i32.highest)))

  let arbitraryi64  : gen i64  =
    -- sized (\n ->
    --          let n' = i64.i32 n
    --          in choose_i64 (-n',n'))
    sized (\n -> choose_i64 (if n < i64.highest
                             then (-n,n)
                             else (i64.lowest, i64.highest)))

  let arbitraryu8  : gen u8  =
    sized (\n ->
             let n' = u8.i64 <| i64.min n <| i64.u8 u8.highest
             in choose_u8 (0, n'))

  let arbitraryu16  : gen u16  =
    sized (\n ->
             let n' = u16.i64 <| i64.min n <| i64.u16 u16.highest
             in choose_u16 (0, n'))

  let arbitraryu32  : gen u32  =
    sized (\n ->
             let n' = u32.i64 <| i64.min n <| i64.u32 u32.highest
             in choose_u32 (0, n'))
    --sized (\n -> choose_u32 (0, u32.i32 n))

  let arbitraryu64  : gen u64  =
    sized (\n -> choose_u64 (0, u64.i64 n))

  let arbitrary_f32_infty : gen f32 =
    elements [f32.from_bits 0x7f800000, f32.from_bits 0xff800000]

  let arbitrary_f32_nan : gen f32 =
    \_ rng ->
      let (rng, x) = rand_u32 (0,1) rng
      let (rng, y) = rand_u32 (1, 0x007fffff) rng
      in (rng, f32.from_bits ((x << 31) & 0x7f800000 & y))

  let arbitrary_f32_subnormal : gen f32 =
    \_ rng ->
      let (rng, x) = rand_u32 (0,1) rng
      let (rng, y) = rand_u32 (0, 0x007fffff) rng
      in (rng, f32.from_bits ((x << 31 ) & y) )

  let arbitrary_f32_normal : gen f32 =
    \size rng ->
      let (rng, x) = rand_u32 (0,u32.min 0xff (u32.i64 size)) rng --is u32.i64 correct?
      let (rng, y) = rand_u32 (0, 0x007fffff) rng
      in (rng, f32.from_bits <| (x << 23) & y)

  let arbitrary_f32 : gen f32 =
  let precision = 9999999999999
  in (\n rng->
        let (rng, b) = rand_i64 (1,precision) rng
        let (rng, a) = rand_i64 ((-n) * b, n * b) rng
        in (rng, f32.i64 a % f32.i64 b))

  let arbitrary_f64_infty : gen f64 =
    elements [f64.from_bits 0x7ff0000000000000, f64.from_bits 0xfff0000000000000]

  let arbitrary_f64_nan : gen f64 =
    \_ rng ->
      let (rng, a) = rand_u64 (0,1) rng
      let (rng, b) = rand_u64 (1, 0xfffffffffffff) rng
      in (rng, f64.from_bits ((a << 63) & 0x7ff0000000000000 & b))

  let arbitrary_f64_subnormal : gen f64 =
    \_ rng ->
      let (rng, a) = rand_u64 (0,1) rng
      let (rng, b) = rand_u64 (0, 0xfffffffffffff) rng
      in (rng, f64.from_bits <| (a  << 63) & b)

  let arbitrary_f64_normal : gen f64 =
    \size rng ->
      let (rng, a) = rand_u64 (0,u64.min 0x7ff (u64.i64 size)) rng -- is u64.i64 correct?
      let (rng, b) = rand_u64 (0, 0xfffffffffffff) rng
      in (rng, f64.from_bits <| (a << 52) & b)

  let arbitrary_f64 : gen f64 =
  let precision = 9999999999999
  in (\n rng ->
        let (rng, b) = rand_i64 (1,precision) rng
        let (rng, a) = rand_i64 ((-n) * b, n * b) rng
        in (rng, f64.i64 a % f64.i64 b))

  let arbitrarytuple 'a 'b (arbitrarya : gen a) (arbitraryb : gen b) : gen (a,b) =
    (\n rng ->
       let (rng, a) = arbitrarya n rng
       let (rng, b) = arbitraryb n rng
       in (rng, (a,b)))

  let arbitrary2tuple = arbitrarytuple

  let arbitrary3tuple 'a 'b 'c
                      (arbitrarya : gen a)
                      (arbitraryb : gen b)
                      (arbitraryc : gen c)
                      : gen (a,b,c) =
    (\n rng ->
       let (rng, a) = arbitrarya n rng
       let (rng, b) = arbitraryb n rng
       let (rng, c) = arbitraryc n rng
       in (rng, (a,b,c)))

  let arbitrary4tuple 'a 'b 'c 'd
                      (arbitrarya : gen a)
                      (arbitraryb : gen b)
                      (arbitraryc : gen c)
                      (arbitraryd : gen d)
                      : gen (a,b,c,d) =
    (\n rng ->
       let (rng, a) = arbitrarya n rng
       let (rng, b) = arbitraryb n rng
       let (rng, c) = arbitraryc n rng
       let (rng, d) = arbitraryd n rng
       in (rng, (a,b,c,d)))

  let arbitrary5tuple 'a 'b 'c 'd 'e
                      (arbitrarya : gen a)
                      (arbitraryb : gen b)
                      (arbitraryc : gen c)
                      (arbitraryd : gen d)
                      (arbitrarye : gen e)
                      : gen (a,b,c,d,e) =
    (\n rng ->
       let (rng, a) = arbitrarya n rng
       let (rng, b) = arbitraryb n rng
       let (rng, c) = arbitraryc n rng
       let (rng, d) = arbitraryd n rng
       let (rng, e) = arbitrarye n rng
       in (rng, (a,b,c,d,e)))

  let two   arb = arbitrary2tuple arb arb
  let three arb = arbitrary3tuple arb arb arb
  let four  arb = arbitrary4tuple arb arb arb arb
  let five  arb = arbitrary5tuple arb arb arb arb arb

  let arbitraryarr 'elm
                   (arbitraryelm : gen elm)
                   (size : size)
                   : gen ([size]elm) =
    (\maxsize rng ->
       let rngs = split_rng size rng
       let (rngs, xs) = unzip (map (arbitraryelm maxsize) rngs)
       in (r.join_rng rngs, xs))

  let arbitrary2darr 'elm
                 (arbitraryelm : gen elm)
                 (size0 : size)
                 (size1 : size)
                 : gen ([size0][size1]elm) =
    (\maxsize rng ->
       let arr1d = arbitraryarr arbitraryelm size1
       let arr2d = arbitraryarr arr1d size0
       in arr2d maxsize rng)

  let arbitrary3darr 'elm
                 (arbitraryelm : gen elm)
                 (size0 : size)
                 (size1 : size)
                 (size2 : size)
                 : gen ([size0][size1][size2]elm) =
    (\maxsize rng ->
       let arr2d = arbitrary2darr arbitraryelm size1 size2
       let arr3d = arbitraryarr arr2d size0
       in arr3d maxsize rng)

  let arbitrary4darr 'elm
                 (arbitraryelm : gen elm)
                 (size0 : size)
                 (size1 : size)
                 (size2 : size)
                 (size3 : size)
                 : gen ([size0][size1][size2][size3]elm) =
    (\maxsize rng ->
       let arr2d = arbitrary2darr arbitraryelm size2 size3
       let arr4d = arbitrary2darr arr2d size0 size1
       in arr4d maxsize rng)

  let arbitrary5darr 'elm
                 (arbitraryelm : gen elm)
                 (size0 : size)
                 (size1 : size)
                 (size2 : size)
                 (size3 : size)
                 (size4 : size)
                 : gen ([size0][size1][size2][size3][size4]elm) =
    (\maxsize rng ->
       let arr3d = arbitrary3darr arbitraryelm size2 size3 size4
       let arr4d = arbitrary2darr arr3d size0 size1
       in arr4d maxsize rng)

}
