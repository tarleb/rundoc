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
   Module      : Text.Rundoc
   Copyright   : Copyright © 2014 Albert Krewinkel
   License     : GNU AGPL, version 3 or above

   Maintainer  : Albert Krewinkel <tarleb@moltkeplatz.de>

Evaluate code in Rundoc code blocks and re-insert the results.
-}
module Text.Rundoc ( rundoc
                   , rundocBlockClass
                   , runShellCode
                   ) where

import           System.IO (hClose, hGetContents)
import           System.Process ( StdStream( CreatePipe )
                                , createProcess, shell, std_out)

import           Text.Pandoc (Attr, Inline(..))

import           Control.Applicative ((<$>), (<$))
import           Data.Default (Default(..))
import           Data.List (foldl', isPrefixOf)
import           Data.Monoid (mempty)

-- | Run code and return the result if a rundoc code block is supplied;
-- otherwise return the argument unaltered.
rundoc :: Inline -> IO Inline
rundoc x@(Code attr code) | isRundocBlock x = evalCodeBlock attr code
rundoc x                                        = return x

isRundocBlock :: Inline -> Bool
isRundocBlock (Code (_,cls,_) _) = rundocBlockClass `elem` cls
isRundocBlock _                  = False

-- | Prefix used for rundoc classes and parameters.
rundocPrefix :: String
rundocPrefix = "rundoc-"

rundocPrefixed :: String -> String
rundocPrefixed = (++) rundocPrefix

-- | Class-name used to mark rundoc blocks.
rundocBlockClass :: String
rundocBlockClass = rundocPrefixed "block"

isRundocOption :: (String, String) -> Bool
isRundocOption (key, _) = rundocPrefix `isPrefixOf` key

rundocOptions :: [(String, String)] -> RundocOptions
rundocOptions = foldl' parseOption def . map rmPrefix . filter isRundocOption
 where rmPrefix (k,v) = (drop (length rundocPrefix) k, v)

data RundocOptions = RundocOptions
  { rundocLanguage   :: Language
  , rundocResultType :: ResultType
  , rundocExports    :: RundocExports
  } deriving (Eq, Show)

instance Default RundocOptions where
  def = RundocOptions
        { rundocLanguage   = UnknownLanguage ""
        , rundocResultType = Replace
        , rundocExports    = ExportResult
        }

parseOption :: RundocOptions -> (String, String) -> RundocOptions
parseOption opts ("language", x) = opts { rundocLanguage   = parseLanguage x }
parseOption opts ("exports",  x) = opts { rundocExports    = parseExport   x }
parseOption opts ("results",  x) = opts { rundocResultType = parseResult   x }
parseOption opts _               = opts

parseLanguage :: String -> Language
parseLanguage "sh" = Shell
parseLanguage x    = UnknownLanguage x

parseExport :: String -> RundocExports
parseExport "result" = ExportResult
parseExport "output" = ExportResult
parseExport "code"   = ExportCode
parseExport "both"   = ExportBoth
parseExport _        = ExportNone

parseResult :: String -> ResultType
parseResult "silent" = Silent
parseResult _        = Replace

-- | Evaluate a code block.
evalCodeBlock :: Attr               -- ^ Block attributes
              -> String             -- ^ Code to evaluate
              -> IO Inline          -- ^ Resulting inline
evalCodeBlock (id',cls,opts) code =
  Code (id', cls', opts') <$> (fmap (resultString mOpts) evalRes)
 where cls'  = filter (/= rundocBlockClass) cls
       opts' = filter (not . isRundocOption) opts
       mOpts = rundocOptions opts
       evalRes = evalCode mOpts code

resultString :: RundocOptions -> Result -> String
resultString RundocOptions{ rundocExports = ExportCode   } = resultCode
resultString RundocOptions{ rundocExports = ExportResult } = resultOutput
resultString RundocOptions{ rundocExports = ExportBoth   } = const mempty
resultString RundocOptions{ rundocExports = ExportNone   } = const mempty

evalCode :: RundocOptions
         -> String
         -> IO Result
evalCode opts code = do
  evalCodeForLang (rundocLanguage opts) code

data Result = Result
  { resultOutput :: String
  , resultValue  :: String
  , resultCode   :: String
  } deriving (Eq, Show)

data ResultType = Replace
                | Silent
                  deriving (Eq, Show)

data RundocExports = ExportCode
                   | ExportResult
                   | ExportBoth
                   | ExportNone
                     deriving (Eq, Show)

instance Default RundocExports where
  def = ExportResult

data Language = Shell
              | UnknownLanguage String
                deriving (Eq, Show)

-- newtype CodeString = CodeString (String

evalCodeForLang :: Language -> String -> IO Result
evalCodeForLang Shell code = runShellCode code
evalCodeForLang (UnknownLanguage lang) code =
  let unknownLang = "UNKNOWN LANGUAGE: " ++ lang
  in return Result { resultOutput = unknownLang
                   , resultValue  = unknownLang
                   , resultCode   = code
                   }

runShellCode :: String -> IO Result
runShellCode cmd = do
  (_, Just hout, _, _) <- createProcess (shell cmd){ std_out = CreatePipe }
  output <- hGetContents hout
  output `seq` Result { resultOutput = output
                      , resultValue  = ""
                      , resultCode   = cmd
                      } <$ hClose hout
