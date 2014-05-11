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

module Metropolis.Worker.Tests ( tests ) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Metropolis.Types
import           Metropolis.Worker

tests :: TestTree
tests = testGroup "Worker Tests" [ shellWorkerTests ]

shellWorkerTests :: TestTree
shellWorkerTests = testGroup "Shell Worker"
  [ testCase "Basic Shell output" $
      let res = emptyResult{ resultOutput = "Hello"
                           , resultSource = "printf Hello"
                           }
      in fmap (== res) (runCode def{ parameterLanguage = Shell } "printf Hello")
           @? "wrong printf result"

  , testCase "Haskell interpreter" $
      let ref = emptyResult{ resultOutput = "42\n"
                           , resultSource = "print (42::Int)"
                           }
          res = runCode def{ parameterLanguage = Haskell } "print (42::Int)"
          resetErr r = r{ resultError = "" }
      in fmap (== ref) (fmap resetErr res)
           @? "Haskell results differ"

  , testCase "Run unknown language" $
      let (lang, code) = (UnknownLanguage "unknown", "foo")
      in fmap (== cannotRunLanguageResult code)
              (runCode def{ parameterLanguage = lang } code)
           @? "wrong fallback"
  ]
