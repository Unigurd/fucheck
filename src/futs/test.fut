--import "testModuleType"
module Test = {
  type t = (i32,i32,i32)
  let gen (a : i32) : t = (a, a+1, a+2)
  let property ((a,b,c) : t) : bool = ((a + b) + c) != (a + (b + c))

}
