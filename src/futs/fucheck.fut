import "lib/github.com/diku-dk/cpprandom/random"
module dist = uniform_int_distribution i32 minstd_rand

type result = #success | #failure i32


let arbitrary (gen : minstd_rand.rng) : [100]i32 = 
  let (_, is) = unzip <| map (dist.rand (1,6)) (minstd_rand.split_rng 100 gen)
  in is

let property  (is : [100]i32) : bool =
  reduce (+) 0 (map (+1) is) == (reduce (+) 0 is + 100)

let runTest 't (arbitrary : minstd_rand.rng -> t) (property : t -> bool) (seed : i32) : bool =
  let gen = minstd_rand.rng_from_seed [seed]
  in property (arbitrary gen)

 --let (rng, x) = dist.rand (1,6) rng
entry entrance (seed : i32) : []u8 = "HejæøåÆØÅ med digÆ"

--entry entrance (seed : i32) : bool = runTest arbitrary property seed
