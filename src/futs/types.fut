module Types = {

  type testdata 't       = #testdata t
  type size              = i32
  type maxtests          = i32
  type maxsize           = size
  type maxdiscardedratio = i32
  type state             = { maxtests : maxtests
                           , maxsize  : maxsize
                           , maxdiscardedratio : maxdiscardedratio }

  let defaultstate : state = {maxtests = 100, maxsize = 100, maxdiscardedratio = 10}
  entry maxtests (state : state) : maxtests =
    state.maxtests
  entry maxsize  (state : state) : maxsize =
    state.maxsize
  entry maxdiscardedratio (state : state) : maxdiscardedratio =
    state.maxdiscardedratio


  let testdata 'n (n : n) : testdata n = #testdata n
  let untestdata 'elm (td : testdata elm) : elm =
    match td
    case #testdata elm -> elm

  let get 'elm (td : testdata elm) : elm =
    match td
    case #testdata elm -> elm

}
