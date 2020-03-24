import "lib/github.com/diku-dk/cpprandom/random"
module dist = uniform_int_distribution i32 minstd_rand


type testdata 't = #testdata t
type result = #success | #failure i32
type gen = minstd_rand.rng
type maybe 'a = #just a | #nothing

let bind 'a 'b (m : maybe a) (f : a -> maybe b) : maybe b = match m
  case #just a  -> f a
  case #nothing -> #nothing

let isJust 't (maybe : maybe t) = match maybe
  case #just _  -> true
  case #nothing -> false

--
-- Generator combinators
--

let geni32range ((low,high) : (i32,i32)) (gen : gen) : (gen,i32) =
  dist.rand(low,high) gen

-- always  generates 0 for some reason
--let geni32 (gen : gen) : (gen,i32) =
--  dist.rand (i32.lowest, i32.highest) gen

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
-- String combinators
--

type sign = #positive | #negative

-- Greatest lower power of exp
let glp (factor : i32) (n :i32) =
  loop i = 1 while (factor ** i) <= n do i + 1

let showBase36 digit : u8 = match digit
  case 0  -> '0'
  case 1  -> '1'
  case 2  -> '2'
  case 3  -> '3'
  case 4  -> '4'
  case 5  -> '5'
  case 6  -> '6'
  case 7  -> '7'
  case 8  -> '8'
  case 9  -> '9'
  case 10 -> 'a'
  case 11 -> 'b'
  case 12 -> 'c'
  case 13 -> 'd'
  case 14 -> 'e'
  case 15 -> 'f'
  case 16 -> 'g'
  case 17 -> 'h'
  case 18 -> 'i'
  case 19 -> 'j'
  case 20 -> 'k'
  case 21 -> 'l'
  case 22 -> 'm'
  case 23 -> 'n'
  case 24 -> 'o'
  case 25 -> 'p'
  case 26 -> 'q'
  case 27 -> 'r'
  case 28 -> 's'
  case 29 -> 't'
  case 30 -> 'u'
  case 31 -> 'v'
  case 32 -> 'w'
  case 33 -> 'x'
  case 34 -> 'y'
  case 35 -> 'z'
  case _  -> '_'



let showSign (sign : sign) = match sign
  case #positive -> ""
  case #negative -> "-"

let digify base n : (sign, []i32) = 
  (if n < 0 then #negative else #positive,
  let n = i32.abs n
  let digitNr = glp base n
  let digitArr = reverse <| iota digitNr
  let (digits, _) =
        loop (digits, remainder) = ([], n) 
        for i in digitArr 
        do (digits ++ [remainder / (base**i)],remainder % (base**i))
  in digits
  )

let num2str prefix stringify base num =
  let (sign, digits) = digify base num
  in showSign sign ++ prefix ++ (map stringify digits)

let showdecimali32 = num2str ""   showBase36 10
let showhexi32     = num2str "0x" showBase36 16
let showoctali32   = num2str "0o" showBase36 8
let showbinaryi32  = num2str "0b" showBase36 2

-- Hvorfor brokker foldl sig over at resultatet ikke har samme stoerrelse som "" ?
--let separatewith separator stringify strs : []u8 = foldl (\x y -> x ++ separator ++ stringify y) "" strs

let separatewith separator stringify arr : []u8 =
  loop str = "" for elm in arr do str ++ separator ++ stringify elm

let surroundwith prefix postfix str = prefix ++ str ++ postfix

let showCollection prefix separator postfix stringify strs = surroundwith prefix postfix <| separatewith separator stringify strs

let show2tuple str1 str2           = "(" ++ str1 ++ ", " ++ str2 ++ ")"
let show3tuple str1 str2 str3      = "(" ++ str1 ++ ", " ++ str2 ++ ", " ++ str3 ++ ")"
let show4tuple str1 str2 str3 str4 = "(" ++ str1 ++ ", " ++ str2 ++ ", " ++ str3 ++ ", " ++ str4 ++ ")"

let showArray stringify strs = showCollection "[" ", " "]" stringify strs

--
-- Generator combinators
--

--shrinkIntegral :: Integral a => a -> [a]
--shrinkIntegral x =
--  nub $
--  [ -x
--  | x < 0, -x > x
--  ] ++
--  [ x'
--  | x' <- takeWhile (<< x) (0:[ x - i | i <- tail (iterate (`quot` 2) x) ])
--  ]
-- where
--   -- a << b is "morally" abs a < abs b, but taking care of overflow.
--   a << b = case (a >= 0, b >= 0) of
--            (True,  True)  -> a < b
--            (False, False) -> a > b
--            (True,  False) -> a + b < 0
--            (False, True)  -> a + b > 0


let countshrunk shrink data =
  loop i = 0 while isJust (shrink data i) do i + 1

let augmentShrink 'a augmentation shrink data i : maybe a =
  if i == 0 then augmentation else shrink data (i-1)

-- Tries all shrinking combinations of (data1, data2) based on the supplied shrinks of a and b
let shrinktogether 's 't
                   (shrink1 : s -> i32 -> maybe s)
                   (shrink2 : t -> i32 -> maybe t)
                   ((data1, data2) : (s, t))
                   (i : i32) 
                   : maybe (s, t) =
  -- Tries shrinking each argument separately. For simplicity
  let n = countshrunk shrink1 data1 -- slow, might be moved
  in if i < n then bind (shrink1 data1 i) (\data1 -> #just (data1, data2)) else
  let i = i - n
  let m = countshrunk shrink2 data2
  in if i < m then bind (shrink2 data2 i) (\data2 -> #just (data1, data2)) else
  if n == 0 || m == 0 then #nothing else
  let i = i - m
  let shrunk1 = shrink1 data1 (i/m)
  let shrunk2 = shrink2 data2 (i%m)
  in match (shrunk1, shrunk2)
     case (#just shrunk1, #just shrunk2)
       -> #just (shrunk1, shrunk2)
     case (_, _)  -> #nothing

--
--  -- If we cannot shrink data2 we handle it separately,
--  -- as we need to divide by n later
--  in if n == 0 then bind (shrink1 data1 i) (\data1 -> #just (data1, data2)) else
--  -- Adds the identity as the first attempted shrinking
--  -- So we also try only shrinking 1 argument
--  let shrink1 = augmentShrink (#just data1) shrink1
--  let shrink2 = augmentShrink (#just data2) shrink2
--  -- Increase i so i = 0 won't 
--  -- leave both data1 and data2 unchanged
--  let i = i + 1
--  let maybeshrunk1 = shrink1 data1 (i/(n+1))
--  let maybeshrunk2 = shrink2 data2 (i%(n+1))
--  in match (maybeshrunk1, maybeshrunk2)
--     case (#just shrunk1, #just shrunk2)
--       -> #just (shrunk1, shrunk2)
--     case (_, _)  -> #nothing


let naiveshrinki32 (data :i32) (i : i32) : maybe i32 =
  let shrunk = data - data / (2**(i-1))
  in if (data < 0  && data >= shrunk)
     || (data >= 0 && data <= shrunk)
     then #nothing
     else #just shrunk

  

let shrinki32 (data : i32) (i : i32) : maybe i32 =
  -- if data is 0 we shrink it naively
  if data == 0 then naiveshrinki32 data i
  -- otherwise we also try shrinking data to 0 as the first thing.
  else let shrink = augmentShrink (#just 0) naiveshrinki32
       in if data > 0 then shrink data i
          -- if data is negative, we also try negating it first
          else augmentShrink (#just (-data)) shrink data i


--
-- Tests
--

let zipGeni32 (gen : gen) : ([]i32, []i32) =
  let (gen, length) = dist.rand (0,1000) gen
  let (gen, arr1)   = genArrLen (geni32range (-100,100)) length gen
  let (_,    arr2)  = genArrLen (geni32range (-100,100)) length gen
  in (arr1, arr2)

let zipTest [n] ((as,bs) : ([n]i32,[n]i32)) = (as,bs) == unzip (zip as bs)

let zipShow _ : []u8 = "not implemented"



let stupidGeni32 gen : testdata (i32, i32)=
  let (gen, i1) = geni32range (-100,00) gen
  let (_,   i2) = geni32range (-100,00) gen
  in #testdata (i1,i2)

let stupidTest (input : testdata (i32,i32)) = match input
  case #testdata (i1, i2) -> i1 != i2

let stupidShow (input : testdata (i32, i32)) = match input
  case #testdata (i1,i2) -> show2tuple (showdecimali32 i1) (showdecimali32 i2)


let stupidShrink = shrinktogether shrinki32 shrinki32


let isZeroShow (input : testdata i32) : []u8 = match input
  case #testdata m -> showdecimali32 m

let isZeroGen gen : testdata i32= let (_, i) = geni32range (-100,100) gen in #testdata i
let isZeroTest (input : testdata i32) = match input
  case #testdata i -> i == 0



--
-- Entry stuff
--

let it 't
        (property : t -> bool)
        (shrink : t -> i32 -> maybe t)
        ((smallest, input, i) : (t, maybe t, i32))
        : (t, maybe t, i32) =
  match input
    -- This case should never be reached because of the loop condition
    case #nothing -> (smallest, #nothing, i)
    case #just inputVal ->
      match shrink inputVal i
      -- We cannot shrink any further
      case #nothing -> (smallest, #nothing, i)
      case #just shrunk ->
        if property shrunk
        -- if the property holds we'll try shrinking further on the same input
        then (smallest, input, i + 1) 
        -- if the property doesn't hold, we'll shrink on the new, smaller input
        else (shrunk, #just shrunk, 0)

let bla p s (x,i) = it p s (x, #just x, i)

let shrinker 't 
             (property : t -> bool)
             (shrink : t -> i32 -> maybe t)
             (input : t)
             : t =
  let (shrunkinput, _, _) = iterate_while (\(_,y,_) -> isJust y) (it property shrink) (input, #just input, 0)
--    loop (smallest, input, i) = (input, #just input, 0)
--    while isJust input
--    do it property shrink (smallest, input, i)
  in shrunkinput

let runTest 't 
            (arbitrary : minstd_rand.rng -> t)
            (property : t -> bool)
            (show : t -> []u8)
--            (shrink : t -> i32 -> maybe t)
            (seed : i32) : 
            (bool,[]u8) =
  let gen          = minstd_rand.rng_from_seed [seed]
  let input        = arbitrary gen
  let result       = property input
  --let shrunkInput = if result then input else shrinker property shrink input
  in (result, if result then "" else show input)

  --loop i = 1 while (factor ** i) <= n do i + 1

--let fullZip (seed : i32) : (bool, []u8) = runTest zipGeni32 zipTest zipShow seed
let fullStupid (seed : i32) : (bool, []u8) = runTest stupidGeni32 stupidTest stupidShow seed
let fullIsZero (seed : i32) : (bool, []u8) = runTest isZeroGen isZeroTest isZeroShow seed

entry main = fullStupid

entry arbitrary (seed : i32) : testdata (i32, i32)=
  stupidGeni32 (minstd_rand.rng_from_seed [seed])

entry property (input : testdata (i32, i32)) : bool = stupidTest input

entry show (input : testdata (i32, i32)) : []u8 = stupidShow input
