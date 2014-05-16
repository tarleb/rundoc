{-
Copyright (C) 2014  Albert Krewinkel <tarleb+metropolis@moltkeplatz.de>

This program is free software: you can redistribute it and/or modify it
under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public
License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

{- |
   Copyright   : Copyright © 2014 Albert Krewinkel
   License     : GNU AGPL, version 3 or above

Eval code in Pandoc code blocks and re-insert the results.
-}
module Main where

import           Text.Rundoc (runBlocksAndInlines)
import           Text.Pandoc.JSON (toJSONFilter)

main :: IO ()
main = toJSONFilter runBlocksAndInlines
