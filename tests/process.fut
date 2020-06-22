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

let process (s1: []i32) (s2: []i32): i32 =
  reduce i32.max 0 (map i32.abs (map2 (-) s1 s2))

let main (s1: []i32) (s2: []i32) : i32 =
  process s1 s2

-- testing with fucheck begins here

let process_gen : i32 -> gen ([](i32, i32)) =
  arbitraryarr (arbitrarytuple arbitraryi32 arbitraryi32)

let process_show  : [](i32, i32) -> []u8 =
  show_array <| showtuple showdecimali32 showdecimali32

-- fucheck proc_comm 1
let gen_proc_comm = process_gen
let prop_proc_comm [n] (arr : ([n](i32, i32))) : bool =
  let (arr0,arr1) = unzip arr
  in process arr0 arr1 == process arr1 arr0

-- fucheck proc_neg 1
let gen_proc_neg = process_gen
let prop_proc_neg [n] (arr : [n](i32, i32)) : bool =
  let (arr0,arr1) = unzip arr
  in process (map (0-) arr0) arr1 == process arr1 (map (0-) arr0)

-- fucheck proc_maxdiff 1
let gen_proc_maxdiff = process_gen
let prop_proc_maxdiff [n] (arr : [n](i32, i32)) : bool =
  let (arr0,arr1) = unzip arr
  let mx = i32.max (reduce i32.max i32.lowest arr0) (reduce i32.max i32.lowest arr1)
  let mn = i32.min (reduce i32.min i32.highest arr0) (reduce i32.min i32.highest arr1)
  let m_diff = mx - mn
  let processed = process arr0 arr1
  in m_diff >= processed

let show_proc_maxdiff = process_show
