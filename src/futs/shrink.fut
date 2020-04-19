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
}
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
  let rng          = minstd_rand.rng_from_seed [seed]
  let input        = arbitrary rng
  let result       = property input
  --let shrunkInput = if result then input else shrinker property shrink input
  in (result, if result then "" else show input)
