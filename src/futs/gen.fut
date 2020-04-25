import "types"
open Types

let untestdata 't (n : testdata t) : t = match n case #testdata m -> m


let snd (_,b) = b

let split_rng_in_2 rng =
  let rngs = minstd_rand.split_rng 2 rng
  in (rngs[0], rngs[1])

--let genify f = {runGen = f}

--let runGen 'a (gen : gen a) (size : size) (rng : rng) : testdata a =
--  match gen
--  case #gen f -> f size rng

let choose (bounds : (i32,i32)) : gen i32 =
   (\_ r -> #testdata (snd (dist.rand bounds r)))

let sized 'a (fgen : size -> gen a) : gen a =
   (\n r -> (fgen n) n r)

let resize 'elm (resizer : size -> size) (oldgen : gen elm) : gen elm =
   (\size rng -> oldgen (resizer size) rng)

let constsize 'elm (newsize : size) (oldgen : gen elm) : gen elm =
   (\_ rng -> oldgen newsize rng)

let scale 'elm (fun : i32 -> i32) (oldgen : gen elm) : gen elm =
   (\size rng -> oldgen (fun size) rng)

let constgen 't (const : t) : gen t =
   (\_ _ -> #testdata const)


let frequencyof2 'elm
                 ((freq0,gen0) : (i32, gen elm))
                 ((freq1,gen1) : (i32, gen elm))
                 : gen elm =
   (\size rng ->
  let (rng0,rng1) = split_rng_in_2 rng
  let totalfreq = freq0 + freq1
        in if (snd (dist.rand (1,totalfreq) rng0)) <= freq0
           then gen0 size rng1
           else gen1 size rng1)

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
  frequencyof2 (freq0 + freq1, frequencyof2 (freq0,gen0) (freq1,gen1))
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
  let i = snd (dist.rand (0,n-1) rng)
        in #testdata elms[i])

-- NOT TESTED
-- Finds the rightmost index at which the element is <= the goal
-- in a sorted array.
-- Doesn't handle the case where all elements are > the goal
let weirdBinarySearch [n] (arr : [n]i32) (goal : i32) : i32 =
  let (_, result, _) =
    loop (lower, current, upper) = (0, n/2, n)
    while lower != current
    do if arr[current] <= goal
       then let next = current + ((upper - current)/2)
            in (current,next,upper)
       else let next = lower + ((current - lower)/2)
            in (lower, next, current)
  in result

-- NOT TESTED
-- Rename to avoid confusion with frequencyofX?
-- assumes all frequencies are > 0
let frequency 'elm [n] (choices : [n](i32,elm)) : gen elm =
  let (freqs,elms) = unzip choices
  let freqsums     = scan (+) 0 freqs
  let total        = freqsums[n-1]
  in  (\_ rng ->
             let goal = (snd (dist.rand (1,total) rng))
             let resultindex = weirdBinarySearch freqsums goal
             in #testdata elms[resultindex])


let arbitrarybool : gen bool =  (\_ rng -> #testdata ((snd (dist.rand (0,1) rng)) == 1))
let arbitraryi32  : gen i32  = sized (\n -> choose (-n,n))



let arbitraryarr 'elm
                 (arbitraryelm : gen elm)
                 : gen ([]elm) =
   (\size rng ->
          let (rng0, rng1) = split_rng_in_2 rng
          let (_,arrSize) = dist.rand (0,size) rng0
          let rngs = minstd_rand.split_rng  arrSize rng1
          in #testdata (map (untestdata <-< arbitraryelm size) rngs))

--let arbitraryarr2 'elm
--                 (arbitraryelm : gen elm)
--                 (ownsize : rng)
--                 : gen ([]elm) =
--   (\sizerng size rng ->
--          let (_,arrSize) = dist.rand (0,size) sizerng
--          let rngs = minstd_rand.split_rng  arrSize rng
--          in #testdata (map (untestdata <-< arbitraryelm size) rngs))

let arbitrarysizedarr 'elm
                      (arbitraryelm : gen elm)
                      (size : size)
                      (rng : rng)
                      : testdata ([size]elm) =
  let rngs = minstd_rand.split_rng size rng
  in #testdata (map (untestdata <-< arbitraryelm size) rngs)

--let arbitrary_arr_of_arr 'elm
--                         --(arbitrary_inner_arr : gen elm -> gen ([]elm))
--                         (arbitraryelm : gen elm)
--                         : gen =
--   (\size rng -> 

let arbitrarytuple 'a 'b (arbitrarya : gen a) (arbitraryb : gen b) : gen (a,b) =
   (\n r ->
          let rngs = minstd_rand.split_rng 2 r
          let a = arbitrarya n rngs[0]
          let b = arbitraryb n rngs[1]
          in match (a,b)
             case (#testdata a, #testdata b) -> #testdata (a,b))

-- instead of arbitrarysortedarray
-- so as not to write a sorting function / add a dependency
-- simply supply an array generator and a sorting function
let transformgen 'a 'b (f : a -> b) (g : gen a) : gen b =
   (\size rng ->
          let b =
            match g size rng
            case #testdata a -> f a
          in #testdata b)

