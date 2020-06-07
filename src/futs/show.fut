import "types"
module Show = {
  open Types

-- Greatest lower power of exp
let glp (factor : u64) (n : u64) =
  loop i = 1 while (factor ** i) <= n do i + 1

  let show_base36 (digit : u64) : u8 =
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


  let digify (base : u64) (n : u64) : []u64 =
  let digitNr = glp base n
  let digitArr = reverse <| map u64.i32 <| iota <| i32.u64 digitNr
  let (digits, _) =
    loop (digits, remainder) = ([], n)
    for i in digitArr
    do (digits ++ [remainder / (base**i)],remainder % (base**i))
  in digits

  let i64_str prefix stringify base num =
  let sign = if num < 0 then "-" else ""
  let digits = digify base <| u64.i64 <| i64.abs num
  in sign ++ prefix ++ (map stringify digits)

  let u64_str prefix stringify base num =
  let digits = digify base num
  in prefix ++ (map stringify digits)

  let showbool b = if get b then "true" else "false"

  let showi64        (num : testdata i64) = i64_str ""   show_base36 10 <| get num
  let showdecimali64 (num : testdata i64) = i64_str ""   show_base36 10 <| get num
  let showhexi64     (num : testdata i64) = i64_str "0x" show_base36 16 <| get num
  let showoctali64   (num : testdata i64) = i64_str "0o" show_base36 8  <| get num
  let showbinaryi64  (num : testdata i64) = i64_str "0b" show_base36 2  <| get num

  let showi32        (num : testdata i32) = showi64        <| map_testdata i64.i32 num
  let showdecimali32 (num : testdata i32) = showdecimali64 <| map_testdata i64.i32 num
  let showhexi32     (num : testdata i32) = showhexi64     <| map_testdata i64.i32 num
  let showoctali32   (num : testdata i32) = showoctali64   <| map_testdata i64.i32 num
  let showbinaryi32  (num : testdata i32) = showbinaryi64  <| map_testdata i64.i32 num

  let showi16        (num : testdata i16) = showi64        <| map_testdata i64.i16 num
  let showdecimali16 (num : testdata i16) = showdecimali64 <| map_testdata i64.i16 num
  let showhexi16     (num : testdata i16) = showhexi64     <| map_testdata i64.i16 num
  let showoctali16   (num : testdata i16) = showoctali64   <| map_testdata i64.i16 num
  let showbinaryi16  (num : testdata i16) = showbinaryi64  <| map_testdata i64.i16 num

  let showi8         (num : testdata i8) = showi64         <| map_testdata i64.i8 num
  let showdecimali8  (num : testdata i8) = showdecimali64  <| map_testdata i64.i8 num
  let showhexi8      (num : testdata i8) = showhexi64      <| map_testdata i64.i8 num
  let showoctali8    (num : testdata i8) = showoctali64    <| map_testdata i64.i8 num
  let showbinaryi8   (num : testdata i8) = showbinaryi64   <| map_testdata i64.i8 num

  let showu64        (num : testdata u64) = u64_str ""   show_base36 10 <| get num
  let showdecimalu64 (num : testdata u64) = u64_str ""   show_base36 10 <| get num
  let showhexu64     (num : testdata u64) = u64_str "0x" show_base36 16 <| get num
  let showoctalu64   (num : testdata u64) = u64_str "0o" show_base36 8  <| get num
  let showbinaryu64  (num : testdata u64) = u64_str "0b" show_base36 2  <| get num

  let showu32        (num : testdata u32) = showu64        <| map_testdata u64.u32 num
  let showdecimalu32 (num : testdata u32) = showdecimalu64 <| map_testdata u64.u32 num
  let showhexu32     (num : testdata u32) = showhexu64     <| map_testdata u64.u32 num
  let showoctalu32   (num : testdata u32) = showoctalu64   <| map_testdata u64.u32 num
  let showbinaryu32  (num : testdata u32) = showbinaryu64  <| map_testdata u64.u32 num

  let showu16        (num : testdata u16) = showu64        <| map_testdata u64.u16 num
  let showdecimalu16 (num : testdata u16) = showdecimalu64 <| map_testdata u64.u16 num
  let showhexu16     (num : testdata u16) = showhexu64     <| map_testdata u64.u16 num
  let showoctalu16   (num : testdata u16) = showoctalu64   <| map_testdata u64.u16 num
  let showbinaryu16  (num : testdata u16) = showbinaryu64  <| map_testdata u64.u16 num

  let showu8         (num : testdata u8) = showu64         <| map_testdata u64.u8 num
  let showdecimalu8  (num : testdata u8) = showdecimalu64  <| map_testdata u64.u8 num
  let showhexu8      (num : testdata u8) = showhexu64      <| map_testdata u64.u8 num
  let showoctalu8    (num : testdata u8) = showoctalu64    <| map_testdata u64.u8 num
  let showbinaryu8   (num : testdata u8) = showbinaryu64   <| map_testdata u64.u8 num

  let separatewith 'elm (separator : []u8) (stringify : testdata elm -> []u8) (testdata_arr : testdata ([]elm)) : []u8 =
    let arr = map testdata <| get testdata_arr in
    if length arr == 0 then ""
    else loop str = stringify (head arr)
         for elm in drop 1 arr
         do str ++ separator ++ stringify elm

  let surroundwith prefix postfix str = prefix ++ str ++ postfix

  let show_collection prefix separator postfix stringify strs =
    surroundwith prefix postfix <| separatewith separator stringify strs

  let showtuple 'elm1 'elm2
                (show1 : testdata elm1 -> []u8)
                (show2 : testdata elm2 -> []u8)
                (tup : testdata (elm1,elm2))
                : []u8 =
    let (x,y) = get tup
    in "(" ++ show1 (testdata x) ++ ", " ++ show2 (testdata y) ++ ")"

  let show2tuple = showtuple

  let show3tuple 'elm1 'elm2 'elm3
               (show1 : testdata elm1 -> []u8)
               (show2 : testdata elm2 -> []u8)
               (show3 : testdata elm3 -> []u8)
               (tup : testdata (elm1,elm2,elm3))
               : []u8 =
    let (x,y,z) = get tup
    in "(" ++ show1 (testdata x) ++ ", "
           ++ show2 (testdata y) ++ ", "
           ++ show3 (testdata z) ++ ")"

  let show4tuple 'elm1 'elm2 'elm3 'elm4
               (show1 : testdata elm1 -> []u8)
               (show2 : testdata elm2 -> []u8)
               (show3 : testdata elm3 -> []u8)
               (show4 : testdata elm4 -> []u8)
               (tup : testdata (elm1,elm2,elm3,elm4))
               : []u8 =
  let (x,y,z,a) = get tup
  in "(" ++ show1 (testdata x) ++ ", "
         ++ show2 (testdata y) ++ ", "
         ++ show3 (testdata z) ++ ", "
         ++ show4 (testdata a) ++ ")"


  let show_array 't (stringify : testdata t -> []u8) (arr : testdata ([]t)) : []u8 =
    show_collection "[" ", " "]" stringify arr

  let show_array2d 't (stringify : testdata t -> []u8) (arr : testdata ([][]t)) : []u8 =
  let inner = show_array stringify
  let outer = show_array inner
  in outer arr

  let show_array3d 't (stringify : testdata t -> []u8) (arr : testdata ([][][]t)) : []u8 =
  let inner  = show_array2d stringify
  let outer  = show_array inner --middle
  in outer arr

let show_array4d 't (stringify : testdata t -> []u8) (arr : testdata ([][][][]t)) : []u8 =
  let inner = show_array3d stringify
  let outer = show_array inner
  in  outer arr

let show_array5d 't (stringify : testdata t -> []u8) (arr : testdata ([][][][][]t)) : []u8 =
  let inner = show_array4d stringify
  let outer = show_array inner
  in  outer arr

let squarebracket n = "[" ++ showdecimali32 n ++ "]"
let show_sizes_1d 'elm [n]
            (_ : testdata ([n]elm))
            : []u8 =
  squarebracket (testdata n)

let show_sizes_2d 'elm [n] [m]
            (_ : testdata([n][m]elm))
            : []u8 =
  squarebracket (testdata n) ++ squarebracket (testdata m)

let show_sizes_3d 'elm [a] [b] [c]
            (_ : testdata ([a][b][c]elm))
            : []u8 =
  squarebracket (testdata a)
                ++ squarebracket (testdata b)
                ++ squarebracket (testdata c)

let show_sizes_4d 'elm [a] [b] [c] [d]
            (_ : testdata ([a][b][c][d]elm))
            : []u8 =
  squarebracket (testdata a)
                ++ squarebracket (testdata b)
                ++ squarebracket (testdata c)
                ++ squarebracket (testdata d)

let show_sizes_5d 'elm [a] [b] [c] [d] [e]
            (_ : testdata ([a][b][c][d][e]elm))
            : []u8 =
  squarebracket (testdata a)
                ++ squarebracket (testdata b)
                ++ squarebracket (testdata c)
                ++ squarebracket (testdata d)
                ++ squarebracket (testdata e)

let show_array4d_crash stringify arr : []u8 =
  let inner = show_array3d stringify
  let outer = show_array inner
  in  outer <| get arr
}



