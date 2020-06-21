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

module Message (crashMessage, formatMessages) where

import Data.List (foldl')
import FutInterface (CInt)

-- make obsolete with new formatting

spaces = ' ':spaces

indent n str =
  take n spaces ++ str

-- off by one?
padEndUntil end str = str ++ take (end - length str) spaces

formatMessages :: String -> [(String, String)] -> [String]
formatMessages separator messages = lines
  where
    (names, values) = unzip messages
    namesColon      = (++ separator) <$> names
    longestName     = foldl' (\acc elm -> max acc $ length elm) 0 namesColon
    formatName      = padEndUntil longestName
    formattedNames  = map formatName namesColon
    lines           = zipWith (++) formattedNames values

funCrash :: String -> [(String,String)] -> [String]
funCrash stage messages = crashMessage
  where
    restLines       = formatMessages ": " messages
    crashMessage    = stage:(indent 2 <$> restLines)

crashMessage :: String -> CInt -> [(String,[(String,String)])] -> [String]
crashMessage name seed messages = crashMessage
  where
    crashLine = ("Property " ++ name ++ " crashed on seed " ++ show seed)
    lines                = uncurry funCrash =<< messages
    linesWithDescription = "in function(s)":(indent 2 <$> lines)
    crashMessage         = crashLine:(indent 2 <$> linesWithDescription)
