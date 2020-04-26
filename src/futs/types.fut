module Types = {
  import "lib/github.com/diku-dk/cpprandom/random"
  module dist = uniform_int_distribution i32 minstd_rand

  type result            = #success | #failure i32 -- Is this used?

  type size              = i32
  type rng               = (minstd_rand.rng, minstd_rand.rng)
  type testdata 't       = #testdata t
  type^ gen 'a           = size ->rng -> testdata a
  type maxtests          = i32
  type maxsize           = i32
  type maxdiscardedratio = i32
  type state             = { maxtests : maxtests
                           , maxsize  : maxsize
                           , maxdiscardedratio : maxdiscardedratio }

}
