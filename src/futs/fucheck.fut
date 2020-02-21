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
  let (gen, res) = dist.rand (0,1) gen
  in  (gen, if res == 1
               then true
               else false)

let genArrLen 't (genElms : (gen -> (gen,t))) (length : i32) (gen : gen) : (gen,[]t) =
  let gens        = minstd_rand.split_rng length gen
  let (gens,elms) = unzip <| map genElms gens
  let gen         = minstd_rand.join_rng gens
  in (gen, elms)

let genArr 't (genElms : (gen -> (gen,t))) (maxLen : i32) (gen : gen) : (gen,[]t) =
  let (gen, length) = dist.rand(0, maxLen) gen
  in genArrLen genElms length gen

--
-- Tests
--

let zipGeni32 (gen : gen) : ([]i32, []i32) =
  let (gen, length) = dist.rand (0,1000) gen
  let (gen, arr1) = genArrLen geni32 length gen
  let (_,    arr2) = genArrLen geni32 length gen
  in (arr1, arr2)

let zipTest [n] ((as,bs) : ([n]i32,[n]i32)) = (as,bs) == unzip (zip as bs)

let zipShow _ : []u8 = "not implemented"


let stupidGeni32 gen =
  let (gen, i1) = geni32range (0,100) gen
  let (_,   i2) = geni32range (0,100) gen
  in (i1,i2)

let stupidTest ((i1,i2) : (i32,i32)) = i1 != i2

let stupidShow (i1,i2) = "ikke implementerET"

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


let fullZip (seed : i32) : (bool, []u8) = runTest zipGeni32 zipTest zipShow seed
let fullStupid (seed : i32) : (bool, []u8) = runTest stupidGeni32 stupidTest stupidShow seed

entry entrance = fullZip
