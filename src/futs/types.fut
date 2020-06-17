module Types = {

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
}
