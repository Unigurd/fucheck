import "rng"

module Gen = {
  open Rng

  type^ gen 'a           = size -> rng -> testdata a



  -- instead of arbitrarysortedarray
  -- so as not to write a sorting function / add a dependency
  -- simply supply an array generator and a sorting function
  let transformgen 'a 'b (f : a -> b) (g : gen a) : gen b =
    (\size rng ->
       let b =
         match g size rng
         case #testdata a -> f a
       in #testdata b)

  let choose_i8 (bounds : (i8,i8)) : gen i8 =
    (\_ r -> #testdata (rand_i8 bounds r))

  let choose_i16 (bounds : (i16,i16)) : gen i16 =
    (\_ r -> #testdata (rand_i16 bounds r))

  let choose_i32 (bounds : (i32,i32)) : gen i32 =
    (\_ r -> #testdata (rand_i32 bounds r))

  let choose_i64 (bounds : (i64,i64)) : gen i64 =
    (\_ r -> #testdata (rand_i64 bounds r))

  let choose_u8 (bounds : (u8,u8)) : gen u8 =
    (\_ r -> #testdata (rand_u8 bounds r))

  let choose_u16 (bounds : (u16,u16)) : gen u16 =
    (\_ r -> #testdata (rand_u16 bounds r))

  let choose_u32 (bounds : (u32,u32)) : gen u32 =
    (\_ r -> #testdata (rand_u32 bounds r))

  let choose_u64 (bounds : (u64,u64)) : gen u64 =
    (\_ r -> #testdata (rand_u64 bounds r))

  let choose_bool (bounds : (bool,bool)) : gen bool =
    (\_ r -> #testdata (rand_bool bounds r))

  let sized 'a (fgen : size -> gen a) : gen a =
    (\n r ->
       match (fgen n)
       case  m -> m n r)

  let resize 'elm (newsize : size) (oldgen : gen elm) : gen elm =
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
       let rngs = split_rng 2 rng
       let totalfreq = freq0 + freq1
       in if rand_i32 (1,totalfreq) rngs[0] <= freq0
          then gen0 size rngs[1]
          else gen1 size rngs[1])

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
       let i = rand_i32 (0,n-1) rng
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
         let goal = rand_i32 (1,total) rng
         let resultindex = weirdBinarySearch freqsums goal
         in #testdata elms[resultindex])



  let arbitrarybool : gen bool =  choose_bool (false,true)

  let arbitraryi8   : gen i8   =
    sized (\n ->
             let n' = i8.i32 n
             in choose_i8 (if n <= i32.i8 i8.highest
                           then (-n',n')
                           else (i8.lowest, i8.highest)))

  let arbitraryi16   : gen i16   =
    sized (\n ->
             let n' = i16.i32 n
             in choose_i16 (if n <= i32.i16 i16.highest
                            then (-n',n')
                            else (i16.lowest, i16.highest)))

  let arbitraryi32  : gen i32  =
    sized (\n -> choose_i32 (if n < i32.highest
                             then (-n,n)
                             else (i32.lowest, i32.highest)))

  let arbitraryi64  : gen i64  =
    sized (\n ->
             let n' = i64.i32 n
             in choose_i64 (-n',n'))

  let arbitraryu8  : gen u8  =
    sized (\n ->
             let n' = u8.i32 <| i32.max n <| i32.u8 u8.highest
             in choose_u8 (0, n'))

  let arbitraryu16  : gen u16  =
    sized (\n ->
             let n' = u16.i32 <| i32.max n <| i32.u16 u16.highest
             in choose_u16 (0, n'))

  let arbitraryu32  : gen u32  =
    sized (\n -> choose_u32 (0, u32.i32 n))

  let arbitraryu64  : gen u64  =
    sized (\n -> choose_u64 (0, u64.i32 n))

  let arbitrary_f32_infty : gen f32 =
    elements [f32.from_bits 0x7f800000, f32.from_bits 0xff800000]

  let arbitrary_f32_nan : gen f32 =
    \_ rng ->
      let rngs = split_rng 2 rng
      in #testdata (f32.from_bits ((rand_u32 (0,1) rngs[0] << 31)
                                   & 0x7f800000
                                   & rand_u32 (1, 0x007fffff) rngs[1]))

  let arbitrary_f32_subnormal : gen f32 =
    \_ rng ->
      let rngs = split_rng 2 rng
      in #testdata (f32.from_bits <| ( rand_u32 (0,1) rngs[0] << 31 )
                                     & rand_u32 (0, 0x007fffff) rngs[1] )

  let arbitrary_f32_normal : gen f32 =
    \size rng ->
      let rngs = split_rng 2 rng
      in #testdata (f32.from_bits <| (rand_u32 (0,u32.min 0xff (u32.i32 size)) rngs[0] << 23)
                                     & rand_u32 (0, 0x007fffff) rngs[1])

  let arbitrary_f32 : gen f32 =
  let precision = 9999999999999
  in (\n rng->
        let rngs = split_rng 2 rng
        let n' = i64.i32 n
        let b = rand_i64 (1,precision) rngs[0]
        let a = rand_i64 ((-n') * b, n' * b) rngs[1]
        in #testdata (f32.i64 a % f32.i64 b))

  let arbitrary_f64_infty : gen f64 =
    elements [f64.from_bits 0x7ff0000000000000, f64.from_bits 0xfff0000000000000]

  let arbitrary_f64_nan : gen f64 =
    \_ rng ->
      let rngs = split_rng 2 rng
      in #testdata (f64.from_bits ((rand_u64 (0,1) rngs[0] << 63)
                                   & 0x7ff0000000000000
                                   & rand_u64 (1, 0xfffffffffffff) rngs[1]))

  let arbitrary_f64_subnormal : gen f64 =
    \_ rng ->
      let rngs = split_rng 2 rng
      in #testdata (f64.from_bits <| ( rand_u64 (0,1) rngs[0] << 63)
                                     & rand_u64 (0, 0xfffffffffffff) rngs[1])

  let arbitrary_f64_normal : gen f64 =
    \size rng ->
      let rngs = split_rng 2 rng
      in #testdata (f64.from_bits <| (rand_u64 (0,u64.min 0x7ff (u64.i32 size)) rngs[0] << 52)
                                     & rand_u64 (0, 0xfffffffffffff) rngs[1])

  let arbitrary_f64 : gen f64 =
  let precision = 9999999999999
  in (\n rng->
        let rngs = split_rng 2 rng
        let n' = i64.i32 n
        let b = rand_i64 (1,precision) rngs[0]
        let a = rand_i64 ((-n') * b, n' * b) rngs[1]
        in #testdata (f64.i64 a % f64.i64 b))

  let arbitrarytuple 'a 'b (arbitrarya : gen a) (arbitraryb : gen b) : gen (a,b) =
    (\n r ->
       let rngs = minstd_rand.split_rng 2 r
       let a = arbitrarya n rngs[0]
       let b = arbitraryb n rngs[1]
       in match (a,b)
          case (#testdata a, #testdata b) -> #testdata (a,b))

  let arbitrary2tuple = arbitrarytuple

  let arbitrary3tuple 'a 'b 'c
                      (arbitrarya : gen a)
                      (arbitraryb : gen b)
                      (arbitraryc : gen c)
                      : gen (a,b,c) =
    (\n r ->
       let rngs = minstd_rand.split_rng 3 r
       let a = arbitrarya n rngs[0]
       let b = arbitraryb n rngs[1]
       let c = arbitraryc n rngs[2]
       in match (a,b,c)
          case (#testdata a, #testdata b, #testdata c) -> #testdata (a,b,c))

  let arbitrary4tuple 'a 'b 'c 'd
                      (arbitrarya : gen a)
                      (arbitraryb : gen b)
                      (arbitraryc : gen c)
                      (arbitraryd : gen d)
                      : gen (a,b,c,d) =
    (\n r ->
       let rngs = minstd_rand.split_rng 4 r
       let a = arbitrarya n rngs[0]
       let b = arbitraryb n rngs[1]
       let c = arbitraryc n rngs[2]
       let d = arbitraryd n rngs[3]
       in match (a,b,c,d)
          case (#testdata a, #testdata b,
                #testdata c, #testdata d)
          -> #testdata (a,b,c,d))

  let arbitrary5tuple 'a 'b 'c 'd 'e
                      (arbitrarya : gen a)
                      (arbitraryb : gen b)
                      (arbitraryc : gen c)
                      (arbitraryd : gen d)
                      (arbitrarye : gen e)
                      : gen (a,b,c,d,e) =
    (\n r ->
       let rngs = minstd_rand.split_rng 5 r
       let a = arbitrarya n rngs[0]
       let b = arbitraryb n rngs[1]
       let c = arbitraryc n rngs[2]
       let d = arbitraryd n rngs[3]
       let e = arbitrarye n rngs[4]
       in match (a,b,c,d,e)
          case (#testdata a, #testdata b,
                #testdata c, #testdata d,
                #testdata e)
          -> #testdata (a,b,c,d,e))

  let two   arb = arbitrary2tuple arb arb
  let three arb = arbitrary3tuple arb arb arb
  let four  arb = arbitrary4tuple arb arb arb arb
  let five  arb = arbitrary5tuple arb arb arb arb arb

  let arbitraryarr 'elm
                 (arbitraryelm : gen elm)
                 (size : size)
                 : gen ([size]elm) =
    (\maxsize rng ->
       let rngs = minstd_rand.split_rng size rng
       in #testdata (map (untestdata <-< arbitraryelm maxsize) rngs))

  let arbitrary2darr 'elm
                 (arbitraryelm : gen elm)
                 (size0 : size)
                 (size1 : size)
                 : gen ([size0][size1]elm) =
    (\maxsize rng ->
       let arr1d = arbitraryarr arbitraryelm size1
       let arr2d = arbitraryarr arr1d size0
       in arr2d maxsize rng)

  let arbitrary3darr 'elm
                 (arbitraryelm : gen elm)
                 (size0 : size)
                 (size1 : size)
                 (size2 : size)
                 : gen ([size0][size1][size2]elm) =
    (\maxsize rng ->
       let arr2d = arbitrary2darr arbitraryelm size1 size2
       let arr3d = arbitraryarr arr2d size0
       in arr3d maxsize rng)

  let arbitrary4darr 'elm
                 (arbitraryelm : gen elm)
                 (size0 : size)
                 (size1 : size)
                 (size2 : size)
                 (size3 : size)
                 : gen ([size0][size1][size2][size3]elm) =
    (\maxsize rng ->
       let arr2d = arbitrary2darr arbitraryelm size2 size3
       let arr4d = arbitrary2darr arr2d size0 size1
       in arr4d maxsize rng)

  let arbitrary5darr 'elm
                 (arbitraryelm : gen elm)
                 (size0 : size)
                 (size1 : size)
                 (size2 : size)
                 (size3 : size)
                 (size4 : size)
                 : gen ([size0][size1][size2][size3][size4]elm) =
    (\maxsize rng ->
       let arr3d = arbitrary3darr arbitraryelm size2 size3 size4
       let arr4d = arbitrary2darr arr3d size0 size1
       in arr4d maxsize rng)

}
