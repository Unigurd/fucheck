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

import "../src/futs//fucheck"
open Fucheck

let ilog2 (x: i32) = 31 - i32.clz x

let work_efficient [n] (xs: [n]i32) : [n]i32 =
  let m = ilog2 n
  let upswept =
    loop xs = copy xs for d in (m-1..m-2...0) do
    let ofs = 2 ** d
    let b   = 2 ** (m - d)
    let k   = 2 ** (m - d - 1)
    let writes = map (\i -> ((i + 1) * b) - 1) (iota ofs)
    let reads = map (\i -> i - k) writes
    let vals = map2(\r w -> xs[r] + xs[w]) reads writes
    in  scatter xs writes vals

  let upswept[n-1] = 0

  let downswept =
    loop xs = copy upswept for d in (0..<m) do
    let ofs = 2 ** d
    let b   = 2 ** (m - d)
    let k   = 2 ** (m - d - 1)
    let red_src = map (\i -> ((i + 1) * b) - 1) (iota ofs)
    let blue_src = map (\i -> i - k) red_src
    let vals = map2(\r w -> xs[r] + xs[w]) blue_src red_src
    let red_vals = map(\r -> xs[r]) red_src
    let xs' = scatter xs red_src vals
    in scatter xs' blue_src red_vals
  in downswept

-- main
let main (s: []i32) : []i32 =
  work_efficient s

-- testing with fucheck begins here

let show (input : ([]i32)) : []u8 =
  show_array showdecimali32 input

let prop (arr : ([]i32)) : bool =
  scan (+) 0 arr ==  work_efficient arr

-- fucheck non_zero_arr 1
let gen_non_zero_arr arrsize : gen ([]i32) =
  arbitraryarr arbitraryi32 (2 ** (1 + ilog2 arrsize))

let prop_non_zero_arr = prop

let show_non_zero_arr = show

-- fucheck with_zero_arr
let gen_with_zero_arr : gen ([0]i32) = constgen []

let prop_with_zero_arr = prop

let show_with_zero_arr = show

