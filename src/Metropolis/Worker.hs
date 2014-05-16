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
   Copyright   : © 2014 Albert Krewinkel <tarleb+metropolis@moltkeplatz.de>
   License     : GNU AGPL, version 3 or above

Run of different languages, producing a unified result type.
-}
module Metropolis.Worker ( runCode
                         , cannotRunLanguageResult
                         ) where

import           Metropolis.Types
import           System.Process ( readProcessWithExitCode )
import           System.IO.Temp
import           System.FilePath

import           Control.Applicative ((<$>), (<*>), pure)
import           Control.Monad (mzero)

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)

runCode :: MetropolisParameter -> CodeString -> IO MetropolisResult
runCode param cmd =
  fromMaybe (return $ cannotRunLanguageResult cmd)
            (interpretCommand param cmd)

interpretCommand :: MetropolisParameter
                 -> CodeString
                 -> Maybe (IO MetropolisResult)
interpretCommand param cmd =
  lookupWorker (parameterLanguage param) <*> pure param <*> pure cmd

cannotRunLanguageResult :: CodeString -> MetropolisResult
cannotRunLanguageResult code =
  emptyResult{ resultSource = code }

type MetropolisWorker = MetropolisParameter -> CodeString -> IO MetropolisResult

lookupWorker :: Language -> Maybe MetropolisWorker
lookupWorker lang = M.lookup lang interpreters

interpreters :: Map Language MetropolisWorker
interpreters = M.fromList
  [ (Shell,   shellWorker)
  , (Haskell, haskellWorker)
  , (Ditaa,   ditaaWorker)
  , (R,       rWorker)
  ]

shellWorker :: MetropolisWorker
shellWorker _ src =
  setResultSource src <$> runCommand "sh" ["-c", src]

haskellWorker :: MetropolisWorker
haskellWorker _ src =
  setResultSource src <$> runCommand "ghc" ["-e", src]

setResultSource :: CodeString -> MetropolisResult -> MetropolisResult
setResultSource src res = res{ resultSource = src }

rWorker :: MetropolisWorker
rWorker _ src =
  runCommandWithInput "R" ["--no-save"] src

ditaaWorker :: MetropolisWorker
ditaaWorker params src =
  flip (maybe mzero) (parameterOutfile params) $ \outfile ->
    withSystemTempDirectory "rundocDitaa" $ \tmpdir -> do
      let infile = tmpdir </> "rundoc-input.ditaa"
      writeFile infile src
      res <- runCommand "ditaa" [ infile, "-o", outfile ]
      return $ setResultFile outfile res
 where setResultFile file res = res{ resultFile = Just file }

resultsFromProcess :: (ExitCode, String, String) -> MetropolisResult
resultsFromProcess (exitCode, stdout, stderr) =
  emptyResult{ resultOutput   = stdout
             , resultError    = stderr
             , resultExitCode = exitCode
             }

runCommand :: FilePath
           -> [String]
           -> IO MetropolisResult
runCommand exec args = do
  resultsFromProcess <$> readProcessWithExitCode exec args ""

runCommandWithInput :: FilePath
                    -> [String]
                    -> String
                    -> IO MetropolisResult
runCommandWithInput exec args input = do
  setResultSource input . resultsFromProcess
                    <$> readProcessWithExitCode exec args input
