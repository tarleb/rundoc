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
module Metropolis.Worker ( runHaskellCommand
                         , runShellCommand
                         , runCommand
                         ) where

import           Metropolis.Types
import           System.IO (hClose, hGetContents)
import           System.Process ( StdStream( CreatePipe )
                                , createProcess, proc, shell, std_out)
import           Control.Applicative ((<$))

runCommand :: Language -> String -> IO MetropolisResult
runCommand Shell                  = runShellCommand
runCommand Haskell                = runHaskellCommand
runCommand (UnknownLanguage lang) = runUnknownLanguageCommand lang

runUnknownLanguageCommand :: String -> String -> IO MetropolisResult
runUnknownLanguageCommand lang code =
  let unknownLang = "UNKNOWN LANGUAGE: " ++ lang
  in return MetropolisResult { metropolisResultOutput = unknownLang
                   , metropolisResultValue  = unknownLang
                   , metropolisResultCode   = code
                   }

runShellCommand :: String -> IO MetropolisResult
runShellCommand cmd = do
  (_, Just hout, _, _) <- createProcess (shell cmd){ std_out = CreatePipe }
  output <- hGetContents hout
  output `seq` MetropolisResult { metropolisResultOutput = output
                      , metropolisResultValue  = ""
                      , metropolisResultCode   = cmd
                      } <$ hClose hout

runHaskellCommand :: String -> IO MetropolisResult
runHaskellCommand cmd = do
  let procParam = (proc "ghc" ["-e", cmd]){ std_out = CreatePipe }
  (_, Just hout, _, _) <- createProcess procParam
  output <- hGetContents hout
  output `seq` MetropolisResult { metropolisResultOutput = output
                      , metropolisResultValue  = ""
                      , metropolisResultCode   = cmd
                      } <$ hClose hout
