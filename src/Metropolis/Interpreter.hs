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
   Module      : Metropolis.Languages
   Copyright   : © 2014 Albert Krewinkel <tarleb@moltkeplatz.de>
   License     : GNU AGPL, version 3 or above

Run of different languages, producing a unified result type.
-}
module Metropolis.Interpreter ( interpreters
                              , lookupInterpreter
                              , shellInterpreter
                              , haskellInterpreter
                              ) where

import           Metropolis.Types
import           System.Process (proc, shell)
import           Control.Applicative ((<$>), (<$))
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)

type CodeString = String

lookupInterpreter :: Language -> Maybe Interpreter
lookupInterpreter lang = M.lookup lang interpreters

interpreters :: Map Language Interpreter
interpreters = M.fromList
  [ (Shell, shellInterpreter)
  , (Haskell, haskellInterpreter)
  ]

shellInterpreter :: Interpreter
shellInterpreter =
  Interpreter
  { interpreterCmd = shell
  }

haskellInterpreter :: Interpreter
haskellInterpreter =
  Interpreter
  { interpreterCmd = \cmd -> proc "ghc" ["-e", cmd]
  }

