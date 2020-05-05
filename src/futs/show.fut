module Show = {
  type sign = #positive | #negative

-- Greatest lower power of exp
let glp (factor : i32) (n :i32) =
  loop i = 1 while (factor ** i) <= n do i + 1

  let showBase36 digit : u8 =
    match digit
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

  let showbool b = if b then "true" else "false"

  let showdecimali32 = num2str ""   showBase36 10
  let showhexi32     = num2str "0x" showBase36 16
  let showoctali32   = num2str "0o" showBase36 8
  let showbinaryi32  = num2str "0b" showBase36 2

  let separatewith 'elm (separator : []u8) (stringify : elm -> []u8) (arr : []elm) : []u8 =
    if length arr == 0 then ""
    else loop str = stringify (head arr)
         for elm in drop 1 arr
         do str ++ separator ++ stringify elm

--let separatewith separator stringify arr : []u8 =
--  loop str = "" for elm in arr do str ++ separator ++ stringify elm

  let separatewith2 'elm (sep0 : []u8) (sep1 : []u8) (interval : i32) stringify (arr : []elm) : []u8 =
    loop str = ""
    for i < length arr
    do str ++ if i % interval == 0 then sep1 else sep0 ++ stringify (arr[i])

  let surroundwith prefix postfix str = prefix ++ str ++ postfix

  let showCollection prefix separator postfix stringify strs =
    surroundwith prefix postfix <| separatewith separator stringify strs

  let showtuple 'elm1 'elm2 (show1 : elm1 -> []u8) (show2 : elm2 -> []u8) ((x,y) : (elm1,elm2)) : []u8 =
    "(" ++ show1 x ++ ", " ++ show2 y ++ ")"

  let show2tuple = showtuple

  let show3tuple 'elm1 'elm2 'elm3
               (show1 : elm1 -> []u8)
               (show2 : elm2 -> []u8)
               (show3 : elm3 -> []u8)
               ((x,y,z) : (elm1,elm2,elm3))
               : []u8 =
    "(" ++ show1 x ++ ", " ++ show2 y ++ show3 z ++ ")"

  let show4tuple 'elm1 'elm2 'elm3 'elm4
               (show1 : elm1 -> []u8)
               (show2 : elm2 -> []u8)
               (show3 : elm3 -> []u8)
               (show4 : elm4 -> []u8)
               ((x,y,z,a) : (elm1,elm2,elm3,elm4))
               : []u8 =
    "(" ++ show1 x ++ ", " ++ show2 y ++ show3 z ++ show4 a ++ ")"

--let showtuple str1 str2 =
--  "(" ++ str1 ++ ", " ++ str2 ++ ")"
--let show2tuple  = showtuple
--let show3tuple str1 str2 str3 =
--  "(" ++ str1 ++ ", " ++ str2 ++ ", " ++ str3 ++ ")"
--let show4tuple str1 str2 str3 str4 =
--  "(" ++ str1 ++ ", " ++ str2 ++ ", " ++ str3 ++ ", " ++ str4 ++ ")"
--let show5tuple str1 str2 str3 str4 str5 =
--  "(" ++ str1 ++ ", " ++ str2 ++ ", " ++ str3 ++ ", " ++ str4 ++ ", " ++ str5 ++ ")"

  let showArray stringify arr : []u8 =
    showCollection "[" ", " "]" stringify arr

--let showArray2d stringify strs : []u8 = separatewith2 "," "], ["

  let showArray2d stringify arr : []u8 =
  let inner = showArray stringify
  let outer = showArray inner
  in outer arr

  let showArray3d stringify arr : []u8 =
  let inner  = showArray stringify
  let middle = showArray inner
  let outer  = showArray middle
  in outer arr

let showArray4d stringify arr : []u8 =
  let a = showArray stringify
  let b = showArray a
  let c = showArray b
  let d = showArray c
  in  d arr

let showArray5d stringify arr : []u8 =
  let a = showArray stringify
  let b = showArray a
  let c = showArray b
  let d = showArray c
  let e = showArray d
  in  e arr

let squarebracket n = "[" ++ showdecimali32 n ++ "]"
let show_sizes_1d 'elm [n]
            (arr : [n]elm)
            : []u8 =
  squarebracket n

let show_sizes_2d 'elm [n] [m]
            (arr : [n][m]elm)
            : []u8 =
  squarebracket n ++ squarebracket m

let show_sizes_3d 'elm [a] [b] [c]
            (arr : [a][b][c]elm)
            : []u8 =
  squarebracket a ++ squarebracket b ++ squarebracket c

let show_sizes_4d 'elm [a] [b] [c] [d]
            (arr : [a][b][c][d]elm)
            : []u8 =
  squarebracket a ++ squarebracket b ++ squarebracket c ++ squarebracket d

let show_sizes_5d 'elm [a] [b] [c] [d] [e]
            (arr : [a][b][c][d][e]elm)
            : []u8 =
  squarebracket a ++ squarebracket b ++ squarebracket c ++ squarebracket d ++ squarebracket e
}
