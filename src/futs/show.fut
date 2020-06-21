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
module Show = {
  open Types

-- Greatest lower power of exp
let glp (factor : u64) (n : u64) =
  loop i = 1 while (factor ** i) <= n do i + 1

  let show_base36 (digit : u64) : u8 =
    match digit
    case 0  -> '0'
    case 1  -> '1'
    case 2  -> '2'
    case 3  -> '3'
    case 4  -> '4'
    case 5  -> '5'
    case 6  -> '6'
    case 7  -> '7'
    case 8  -> '8'
    case 9  -> '9'
    case 10 -> 'a'
    case 11 -> 'b'
    case 12 -> 'c'
    case 13 -> 'd'
    case 14 -> 'e'
    case 15 -> 'f'
    case 16 -> 'g'
    case 17 -> 'h'
    case 18 -> 'i'
    case 19 -> 'j'
    case 20 -> 'k'
    case 21 -> 'l'
    case 22 -> 'm'
    case 23 -> 'n'
    case 24 -> 'o'
    case 25 -> 'p'
    case 26 -> 'q'
    case 27 -> 'r'
    case 28 -> 's'
    case 29 -> 't'
    case 30 -> 'u'
    case 31 -> 'v'
    case 32 -> 'w'
    case 33 -> 'x'
    case 34 -> 'y'
    case 35 -> 'z'
    case _  -> '_'


  let digify (base : u64) (n : u64) : []u64 =
  let digitNr = glp base n
  let digitArr = reverse <| map u64.i32 <| iota <| i32.u64 digitNr
  let (digits, _) =
    loop (digits, remainder) = ([], n)
    for i in digitArr
    do (digits ++ [remainder / (base**i)],remainder % (base**i))
  in digits

  let i64_str prefix stringify base num =
  let sign = if num < 0 then "-" else ""
  let digits = digify base <| u64.i64 <| i64.abs num
  in sign ++ prefix ++ (map stringify digits)

  let u64_str prefix stringify base num =
  let digits = digify base num
  in prefix ++ (map stringify digits)

  let showbool b = if b then "true" else "false"

  let showi64        (num : i64) = i64_str ""   show_base36 10 <| num
  let showdecimali64 (num : i64) = i64_str ""   show_base36 10 <| num
  let showhexi64     (num : i64) = i64_str "0x" show_base36 16 <| num
  let showoctali64   (num : i64) = i64_str "0o" show_base36 8  <| num
  let showbinaryi64  (num : i64) = i64_str "0b" show_base36 2  <| num

  let showi32        (num : i32) = showi64        <| i64.i32 num
  let showdecimali32 (num : i32) = showdecimali64 <| i64.i32 num
  let showhexi32     (num : i32) = showhexi64     <| i64.i32 num
  let showoctali32   (num : i32) = showoctali64   <| i64.i32 num
  let showbinaryi32  (num : i32) = showbinaryi64  <| i64.i32 num

  let showi16        (num : i16) = showi64        <| i64.i16 num
  let showdecimali16 (num : i16) = showdecimali64 <| i64.i16 num
  let showhexi16     (num : i16) = showhexi64     <| i64.i16 num
  let showoctali16   (num : i16) = showoctali64   <| i64.i16 num
  let showbinaryi16  (num : i16) = showbinaryi64  <| i64.i16 num

  let showi8         (num : i8) = showi64         <| i64.i8 num
  let showdecimali8  (num : i8) = showdecimali64  <| i64.i8 num
  let showhexi8      (num : i8) = showhexi64      <| i64.i8 num
  let showoctali8    (num : i8) = showoctali64    <| i64.i8 num
  let showbinaryi8   (num : i8) = showbinaryi64   <| i64.i8 num

  let showu64        (num : u64) = u64_str ""   show_base36 10 <| num
  let showdecimalu64 (num : u64) = u64_str ""   show_base36 10 <| num
  let showhexu64     (num : u64) = u64_str "0x" show_base36 16 <| num
  let showoctalu64   (num : u64) = u64_str "0o" show_base36 8  <| num
  let showbinaryu64  (num : u64) = u64_str "0b" show_base36 2  <| num

  let showu32        (num : u32) = showu64        <| u64.u32 num
  let showdecimalu32 (num : u32) = showdecimalu64 <| u64.u32 num
  let showhexu32     (num : u32) = showhexu64     <| u64.u32 num
  let showoctalu32   (num : u32) = showoctalu64   <| u64.u32 num
  let showbinaryu32  (num : u32) = showbinaryu64  <| u64.u32 num

  let showu16        (num : u16) = showu64        <| u64.u16 num
  let showdecimalu16 (num : u16) = showdecimalu64 <| u64.u16 num
  let showhexu16     (num : u16) = showhexu64     <| u64.u16 num
  let showoctalu16   (num : u16) = showoctalu64   <| u64.u16 num
  let showbinaryu16  (num : u16) = showbinaryu64  <| u64.u16 num

  let showu8         (num : u8) = showu64         <| u64.u8 num
  let showdecimalu8  (num : u8) = showdecimalu64  <| u64.u8 num
  let showhexu8      (num : u8) = showhexu64      <| u64.u8 num
  let showoctalu8    (num : u8) = showoctalu64    <| u64.u8 num
  let showbinaryu8   (num : u8) = showbinaryu64   <| u64.u8 num

  let separatewith 'elm (separator : []u8) (stringify : elm -> []u8) (arr : []elm) : []u8 =
    if length arr == 0 then ""
    else loop str = stringify (head arr)
         for elm in drop 1 arr
         do str ++ separator ++ stringify elm

  let surroundwith prefix postfix str = prefix ++ str ++ postfix

  let show_collection prefix separator postfix stringify strs =
    surroundwith prefix postfix <| separatewith separator stringify strs

  let showtuple 'elm1 'elm2
                (show1 : elm1 -> []u8)
                (show2 : elm2 -> []u8)
                ((x,y) : (elm1,elm2))
                : []u8 =
    "(" ++ show1 x ++ ", " ++ show2 y ++ ")"

  let show2tuple = showtuple

  let show3tuple 'elm1 'elm2 'elm3
               (show1 : elm1 -> []u8)
               (show2 : elm2 -> []u8)
               (show3 : elm3 -> []u8)
               ((x,y,z) : (elm1,elm2,elm3))
               : []u8 =
    "(" ++ show1 x ++ ", "
           ++ show2 y ++ ", "
           ++ show3 z ++ ")"

  let show4tuple 'elm1 'elm2 'elm3 'elm4
               (show1 : elm1 -> []u8)
               (show2 : elm2 -> []u8)
               (show3 : elm3 -> []u8)
               (show4 : elm4 -> []u8)
               ((x,y,z,a) : (elm1,elm2,elm3,elm4))
               : []u8 =
  "(" ++ show1 x ++ ", "
         ++ show2 y ++ ", "
         ++ show3 z ++ ", "
         ++ show4 a ++ ")"


  let show_array 't (stringify : t -> []u8) (arr : []t) : []u8 =
    show_collection "[" ", " "]" stringify arr

  let show_array2d 't (stringify : t -> []u8) (arr : [][]t) : []u8 =
  let inner = show_array stringify
  let outer = show_array inner
  in outer arr

  let show_array3d 't (stringify : t -> []u8) (arr : [][][]t) : []u8 =
  let inner  = show_array2d stringify
  let outer  = show_array inner --middle
  in outer arr

let show_array4d 't (stringify : t -> []u8) (arr : [][][][]t) : []u8 =
  let inner = show_array3d stringify
  let outer = show_array inner
  in  outer arr

let show_array5d 't (stringify : t -> []u8) (arr : [][][][][]t) : []u8 =
  let inner = show_array4d stringify
  let outer = show_array inner
  in  outer arr

let squarebracket n = "[" ++ showdecimali32 n ++ "]"
let show_sizes_1d 'elm [n]
            (_ : [n]elm)
            : []u8 =
  squarebracket n

let show_sizes_2d 'elm [n] [m]
            (_ : [n][m]elm)
            : []u8 =
  squarebracket n ++ squarebracket m

let show_sizes_3d 'elm [a] [b] [c]
            (_ : [a][b][c]elm)
            : []u8 =
  squarebracket a ++ squarebracket b ++ squarebracket c

let show_sizes_4d 'elm [a] [b] [c] [d]
            (_ : [a][b][c][d]elm)
            : []u8 =
  squarebracket a ++ squarebracket b ++ squarebracket c ++ squarebracket d

let show_sizes_5d 'elm [a] [b] [c] [d] [e]
            (_ : [a][b][c][d][e]elm)
            : []u8 =
  squarebracket a ++ squarebracket b ++ squarebracket c ++ squarebracket d ++ squarebracket e

let show_array4d_crash stringify arr : []u8 =
  let inner = show_array3d stringify
  let outer = show_array inner
  in  outer arr
}



