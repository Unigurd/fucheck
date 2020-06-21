-- Copyright (C) Sigurd Dam Sonniks

-- This file is part of Fucheck.

--     Fucheck is free software: you can redistribute it and/or modify
--     it under the terms of the GNU General Public License as published by
--     the Free Software Foundation, either version 3 of the License, or
--     (at your option) any later version.

--     Fucheck is distributed in the hope that it will be useful,
--     but WITHOUT ANY WARRANTY; without even the implied warranty of
--     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--     GNU General Public License for more details.

--     You should have received a copy of the GNU General Public License
--     along with Fucheck.  If not, see <https://www.gnu.org/licenses/>.

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
