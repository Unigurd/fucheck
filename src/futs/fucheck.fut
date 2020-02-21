import "lib/github.com/diku-dk/cpprandom/random"
module dist = uniform_int_distribution i32 minstd_rand


type result = #success | #failure i32
type gen = minstd_rand.rng

--
-- Generator combinators
--

let geni32range ((low,high) : (i32,i32)) (gen : gen) : (gen,i32) =
  dist.rand(low,high) gen

let geni32 (gen : gen) : (gen,i32) = dist.rand (i32.lowest, i32.highest) gen

let genbool (gen : gen) : (gen,bool) =
  let (newGen, res) = dist.rand (0,1) gen
  in  (newGen, if res == 1
               then true
               else false)

let genArr 't (genElms : (gen -> (gen,t))) (maxLen : i32) (gen0 : gen) : (gen,[]t) =
  let (gen1, length) = dist.rand(0, maxLen) gen0
  let gens2 = minstd_rand.split_rng length gen1
  let (gens3,elms) = unzip <| map genElms gens2
  let gen4 = minstd_rand.join_rng gens3
  in (gen4, elms)


--
-- Tests
--

let zipGeni32 (gen0 : gen) : ([]i32, []i32) =
  let (gen1, arr1) = genArr geni32 1000 gen0
  let (_,    arr2) = genArr geni32 1000 gen1
  in (arr1, arr2)

let zipTest [n] ((as,bs) : ([n]i32,[n]i32)) = (as,bs) == unzip (zip as bs)

let zipShow _ : []u8 = "not implemented"


--
-- Entry stuff
--

let runTest 't 
            (arbitrary : minstd_rand.rng -> t)
            (property : t -> bool)
            (show : t -> []u8)
            (seed : i32) : 
            (bool,[]u8) =
  let gen = minstd_rand.rng_from_seed [seed]
  let input = arbitrary gen
  let result = property input
  in (result, if result then "" else show input)


entry entrance (seed : i32) : (bool, []u8) = runTest zipGeni32 zipTest zipShow seed
