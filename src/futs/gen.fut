
import "types"
open Types


let snd (a,b) = b

let runGen 'a (gen : gen a) (size : size) (rng : rng) : testdata a =
  match gen
  case #gen f -> f size rng

let choose (bounds : (i32,i32)) : gen i32 =
  #gen (\_ r -> #testdata (snd (dist.rand bounds r)))

let sized 'a (fgen : size -> gen a) : gen a =
  #gen (\n r ->
          match (fgen n)
          case #gen m -> m n r)

let arbitraryi32 : gen i32 = sized (\n -> choose (-n,n))

let arbitrarytuple 'a 'b (arbitrarya : gen a) (arbitraryb : gen b) : gen (a,b) =
  #gen (\n r ->
          let rngs = minstd_rand.split_rng 2 r
          let a = runGen arbitrarya n rngs[0]
          let b = runGen arbitraryb n rngs[1]
          in match (a,b)
             case (#testdata a, #testdata b) -> #testdata (a,b))

let rngi32range ((low,high) : (i32,i32)) (rng : rng) : (rng,i32) =
  dist.rand(low,high) rng

let rngbool (rng : rng) : (rng,bool) =
  let (rng, res) = dist.rand (0,1) rng
  in  (rng, if res == 1
               then true
               else false)

let rngArrLen 't (rngElms : (rng -> (rng,t))) (length : i32) (rng : rng) : (rng,[]t) =
  let rngs        = minstd_rand.split_rng length rng
  let (rngs,elms) = unzip <| map rngElms rngs
  let rng         = minstd_rand.join_rng rngs
  in (rng, elms)

let rngArr 't (rngElms : (rng -> (rng,t))) (maxLen : i32) (rng : rng) : (rng,[]t) =
  let (rng, length) = dist.rand(0, maxLen) rng
  in rngArrLen rngElms length rng
