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

let main [n] (arr: [n]i32) (arrB: [n]bool) : []i32 =
  segreduce (+) 0 (zip arr arrB)

-- testing with fucheck begins here

let fst (a,_) = a
let snd (_,b) = b

let crude_segreduce [n] (op: i32 -> i32 -> i32) (ne: i32) (arr: [n](i32,bool)) : []i32 =
  let (right,almostdone) =
    loop (right, newarr) = (length arr,[])
    for i in reverse <| iota <| length arr do
      if snd arr[i] then
        (i, [reduce op ne <| drop i <| take right <| fst <| unzip arr] ++ newarr)
      else (right, newarr)
  let result =
    if right == 0 then almostdone else (scan op ne <| take right <| fst <| unzip arr) ++ almostdone
  in result

let fixfirst a =
  if length a == 0 then a else
  let b = copy a
  in b with [0] = (fst a[0],true)

let prop [n] (arr : [n](i32,bool)) : bool =
  let a = crude_segreduce (+) 0 arr
  let b = segreduce (+) 0 arr
  let la = length a
  in if la == length b then
       (a :> [la]i32) == (b :>[la]i32)
     else false

let show [n] (input : [n](i32,bool)) : []u8 =
  show_array (showtuple showdecimali32 showbool) input

-- fucheck segreduce 1
let gen_segreduce arrsize : gen ([arrsize](i32,bool)) =
  let arrgen = arbitraryarr (arbitrarytuple arbitraryi32 arbitrarybool) arrsize
  in (transformgen fixfirst arrgen)

let prop_segreduce = prop
let show_segreduce = show

-- fucheck segreduce_undefined 1
let gen_segreduce_undefined arrsize : gen ([arrsize](i32,bool)) =
  arbitraryarr (arbitrarytuple arbitraryi32 arbitrarybool) arrsize

let prop_segreduce_undefined = prop
let show_segreduce_undefined = show
