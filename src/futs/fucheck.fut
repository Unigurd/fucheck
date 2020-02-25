import "lib/github.com/diku-dk/cpprandom/random"
module dist = uniform_int_distribution i32 minstd_rand


type result = #success | #failure i32
type gen = minstd_rand.rng
type maybe 'a = #just a | #nothing

let isSomething 't (maybe : maybe t) = match maybe
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

entry digify base n : (sign, []i32) = 
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

let showdecimali32 num = num2str ""   showBase36 10 num
let showhexi32     num = num2str "0x" showBase36 16 num
let showoctali32   num = num2str "0o" showBase36 8  num
let showbinaryi32  num = num2str "0b" showBase36 2  num

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



let shrinki32 (data : i32) (i : i32) : maybe i32 =
  -- Don't negate if data is positive
  -- or the lowest possible value,
  -- since -(i32.lowest) = i32.highest + 1,
  -- so it can't be negated
  let i = if data >= 0 || data == i32.lowest then i + 1 else i
  in match i
     case 0 -> #just (-data)
     case 1 -> #just 0
     case _ -> let shrunk = data - data / (2**(i-1))
               in if (data < 0  && data >= shrunk)
                  || (data >= 0 && data <= shrunk)
                  then #nothing
                  else #just shrunk

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


let stupidGeni32 gen =
  let (gen, i1) = geni32range (-100,00) gen
  let (_,   i2) = geni32range (-100,00) gen
  in (i1,i2)

let stupidTest ((i1,i2) : (i32,i32)) = i1 != i2

let stupidShow (i1,i2) = show2tuple (showdecimali32 i1) (showdecimali32 i2)

let isZeroGen gen = let (_, i) = geni32range (-100,100) gen in i
let isZeroTest (i : i32) = i == 0



--
-- Entry stuff
--


let shrinker 't 
             (property : t -> bool)
             (shrink : t -> i32 -> maybe t)
             (input : t)
             : t =
  let (shrunkinput, _, _) = 
    loop (smallest, current, i) = (input, #just input, 0)
    while isSomething current
    do match current
       -- This case should never be reached because of the loop condition
       case #nothing -> (smallest, current, i)
       case #just currentVal ->
         match shrink currentVal i
         -- We cannot shrink any further
         case #nothing -> (smallest, #nothing, i)
         case #just shrunk ->
           if property shrunk
           -- if the property holds we'll try shrinking further on the same input
           then (smallest, current, i + 1) 
           -- if the property doesn't hold, we'll shrink on the new, smaller input
           else (shrunk, #just shrunk, 0)
  in shrunkinput

let runTest 't 
            (arbitrary : minstd_rand.rng -> t)
            (property : t -> bool)
            (show : t -> []u8)
            (shrink : t -> i32 -> maybe t)
            (seed : i32) : 
            (bool,[]u8) =
  let gen          = minstd_rand.rng_from_seed [seed]
  let input        = arbitrary gen
  let result       = property input
  let shrunkInput = if result then input else shrinker property shrink input
  in (result, if result then "" else show shrunkInput)

  --loop i = 1 while (factor ** i) <= n do i + 1

--let fullZip (seed : i32) : (bool, []u8) = runTest zipGeni32 zipTest zipShow seed
--let fullStupid (seed : i32) : (bool, []u8) = runTest stupidGeni32 stupidTest stupidShow seed
let fullisZero (seed : i32) : (bool, []u8) = runTest isZeroGen isZeroTest showdecimali32 shrinki32 seed

entry main = fullisZero
