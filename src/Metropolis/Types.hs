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
                        , MetropolisParameter(..)
                        , Worker(..)
                        , CodeString
                        , ExitCode(..)
                        , Default(def)
                        , emptyResult
                        ) where

import           System.Exit ( ExitCode(..) )

import           Data.Default (Default(def))
import           Data.Monoid (mempty)

type CodeString = String

-- | Metropolis parameters
data MetropolisParameter =
  MetropolisParameter
  { parameterVariables :: [(String, String)]
  , parameterOutfile   :: Maybe FilePath
  , parameterLanguage  :: Language
  } deriving (Eq, Show)

instance Default MetropolisParameter where
  def = MetropolisParameter
        { parameterVariables = []
        , parameterOutfile   = Nothing
        , parameterLanguage  = UnknownLanguage "unspecified"
        }

-- | Results of a computation.
data MetropolisResult = MetropolisResult
  { resultOutput   :: String
  , resultError    :: String
  , resultFile     :: Maybe FilePath
  , resultExitCode :: ExitCode
  , resultSource   :: String
  } deriving (Eq, Show)

emptyResult :: MetropolisResult
emptyResult =
  MetropolisResult
  { resultOutput   = mempty
  , resultError    = mempty
  , resultFile     = mempty
  , resultExitCode = ExitSuccess
  , resultSource   = mempty
  }

data Language = Ditaa
              | Haskell
              | R
              | Shell
              | UnknownLanguage String
                deriving (Eq, Ord, Show)

newtype Worker = Worker { unWorker :: MetropolisParameter
                                   -> String
                                   -> MetropolisResult
                        }
