import "/home/sigurd/studie/bachelor/fucheck/src/futs/fucheck"
open Fucheck

-- ==
-- input @ red_100.in
-- input @ red_1000.in
-- input @ red_10000.in
-- input @ red_100000.in
-- input @ red_1000000.in
-- input @ red_10000000.in

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

let prop [n] (input : testdata ([n](i32,bool))) : bool =
  match input
  case #testdata arr ->
    let a = crude_segreduce (+) 0 arr
    let b = segreduce (+) 0 arr
    let la = length a
    in if la == length b then
         (a :> [la]i32) == (b :>[la]i32)
       else false

let show [n] (input : testdata ([n](i32,bool))) : []u8 =
  show_array (showtuple showdecimali32 showbool) input

-- fucheck segreduce
let gen_segreduce (size : i32) (seed : i32) : testdata ([](i32,bool)) =
  let rngs = split_rng 2 <| rng_from_seed seed
  let sizes = getsizes size rngs[0] 1
  let arrgen = arbitraryarr (arbitrarytuple arbitraryi32 arbitrarybool) sizes[0]
  in (transformgen fixfirst arrgen) size rngs[1]

let prop_segreduce = prop
let show_segreduce = show

-- fucheck segreduce_undefined
let gen_segreduce_undefined (size : i32) (seed : i32) : testdata ([](i32,bool)) =
  let rngs = split_rng 2 <| rng_from_seed seed
  let sizes = getsizes size rngs[0] 1
  let arrgen = arbitraryarr (arbitrarytuple arbitraryi32 arbitrarybool) sizes[0]
  in arrgen size rngs[1]

let prop_segreduce_undefined = prop
let show_segreduce_undefined = show
