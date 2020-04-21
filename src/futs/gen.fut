import "types"
open Types


let snd (_,b) = b

let split_rng_in_2 rng =
  let rngs = minstd_rand.split_rng 2 rng
  in (rngs[0], rngs[1])

let runGen 'a (gen : gen a) (size : size) (rng : rng) : testdata a =
  match gen
  case #gen f -> f size rng

let choose (bounds : (i32,i32)) : gen i32 =
  #gen (\_ r -> #testdata (snd (dist.rand bounds r)))

let sized 'a (fgen : size -> gen a) : gen a =
  #gen (\n r ->
          match (fgen n)
          case #gen m -> m n r)

let resize 'elm (newsize : i32) (oldgen : gen elm) : gen elm =
  #gen (\_ rng -> runGen oldgen newsize rng)


let scale 'elm (fun : i32 -> i32) (oldgen : gen elm) : gen elm =
  #gen (\size rng -> runGen oldgen (fun size) rng)

let frequencyof2 'elm ((freq0,gen0) : (i32, gen elm)) ((freq1,gen1) : (i32, gen elm)) : gen elm =
  #gen (\size rng ->
  let (rng0,rng1) = split_rng_in_2 rng
  let totalfreq = freq0 + freq1
        in if (snd (dist.rand (1,totalfreq) rng0)) <= freq0
           then runGen gen0 size rng1
           else runGen gen1 size rng1)

let oneof2 'elm (gen0 : gen elm) (gen1 : gen elm) : gen elm =
  frequencyof2 (1,gen0) (1,gen1)

let frequencyof3 'elm ((freq0,gen0) : (i32, gen elm)) ((freq1,gen1) : (i32, gen elm)) ((freq2,gen2) : (i32, gen elm)) : gen elm =
  #gen (\size rng ->
  let (rng0,rng1) = split_rng_in_2 rng
  let totalfreq = freq0 + freq1 + freq2
  let value = snd (dist.rand (1,totalfreq) rng0)
        in if value <= freq0
           then runGen gen0 size rng1
           else
           if value <= freq0 + freq1
           then runGen gen1 size rng1
           else runGen gen2 size rng1)

let oneof3 'elm (gen0 : gen elm) (gen1 : gen elm) (gen2 : gen elm) : gen elm =
  frequencyof3 (1,gen0) (1,gen1) (1,gen2)


let elements 'elm [n] (elms : [n]elm) : gen elm =
  #gen (\_ rng ->
  let i = snd (dist.rand (0,n-1) rng)
        in #testdata elms[i])

let arbitrarybool : gen bool = #gen (\_ rng -> #testdata ((snd (dist.rand (0,1) rng)) == 1))
let arbitraryi32  : gen i32  = sized (\n -> choose (-n,n))


--let arbitraryarr 'elm (arbitraryelm : gen elm) : gen ([]elm) =
--  #gen (\size rng ->
--          let (rng0, rng1) = split_rng_in_2 rng
--          let (_,arrSize) = dist.rand (0,size) rng0
--          let rngs = minstd_rand.split_rng  arrSize rng1
--          in #testdata (map (runGen arbitraryelm size) rngs))

let arbitrarytuple 'a 'b (arbitrarya : gen a) (arbitraryb : gen b) : gen (a,b) =
  #gen (\n r ->
          let rngs = minstd_rand.split_rng 2 r
          let a = runGen arbitrarya n rngs[0]
          let b = runGen arbitraryb n rngs[1]
          in match (a,b)
             case (#testdata a, #testdata b) -> #testdata (a,b))


let rngArrLen 't (rngElms : (rng -> (rng,t))) (length : i32) (rng : rng) : (rng,[]t) =
  let rngs        = minstd_rand.split_rng length rng
  let (rngs,elms) = unzip <| map rngElms rngs
  let rng         = minstd_rand.join_rng rngs
  in (rng, elms)

let rngArr 't (rngElms : (rng -> (rng,t))) (maxLen : i32) (rng : rng) : (rng,[]t) =
  let (rng, length) = dist.rand(0, maxLen) rng
  in rngArrLen rngElms length rng

let bla 'elm (arbitraryelm : i32 -> minstd_rand.rng -> elm)
        : i32 -> minstd_rand.rng -> ([]elm) =
  (\size rng ->
     let (rng0, rng1) = split_rng_in_2 rng
     let (_,arrSize) = dist.rand (0,size) rng0
     let rngs = minstd_rand.split_rng  arrSize rng1
     in ((map (arbitraryelm size) rngs)))
