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
   Module      : Metropolis.Types
   Copyright   : © 2014 Albert Krewinkel <tarleb+metropolis@moltkeplatz.de>
   License     : GNU AGPL, version 3 or above

Types for Metropolis, a library for universal code execution.
-}
module Metropolis.Types ( Language(..)
                        , MetropolisResult(..)
                        ) where

-- | Results of a computation.
data MetropolisResult = MetropolisResult
  { metropolisResultOutput :: String
  , metropolisResultValue  :: String
  , metropolisResultCode   :: String
  } deriving (Eq, Show)

data Language = Shell
              | Haskell
              | UnknownLanguage String
                deriving (Eq, Show)
