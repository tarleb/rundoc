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
   Copyright   : © 2014 Albert Krewinkel <tarleb+metropolis@moltkeplatz.de>
   License     : GNU AGPL, version 3 or above

Evaluate code in Rundoc code blocks and re-insert the results.
-}
module Text.Rundoc ( rundoc
                   , rundocBlockClass
                   ) where

import           Metropolis.Types (MetropolisResult(..), Language(..))
import           Metropolis.Worker (runCommand)

import           Text.Pandoc (Attr, Inline( Code, Str ))

import           Control.Applicative ((<$>))
import           Data.Bifunctor (bimap)
import           Data.Default (Default(..))
import           Data.List (foldl', isPrefixOf)
import           Data.Monoid (mempty)

-- | Run code and return the result if a rundoc code block is supplied;
-- otherwise return the argument unaltered.
rundoc :: Inline -> IO Inline
rundoc x@(Code attr code) | isRundocBlock x = evalCodeBlock attr code
rundoc x                                    = return x

isRundocBlock :: Inline -> Bool
isRundocBlock (Code (_,cls,_) _) = rundocBlockClass `elem` cls
isRundocBlock _                  = False

-- | Prefix used for rundoc classes and parameters.
rundocPrefix :: String
rundocPrefix = "rundoc-"

-- | Class-name used to mark rundoc blocks.
rundocBlockClass :: String
rundocBlockClass = rundocPrefix ++ "block"

isRundocOption :: (String, String) -> Bool
isRundocOption (key, _) = rundocPrefix `isPrefixOf` key

-- Options
data RundocOptions = RundocOptions
  { rundocLanguage   :: Language
  , rundocResultType :: RundocResultType
  , rundocExports    :: RundocExports
  } deriving (Eq, Show)

instance Default RundocOptions where
  def = RundocOptions
        { rundocLanguage   = UnknownLanguage ""
        , rundocResultType = Replace
        , rundocExports    = ExportResult
        }

data RundocResultType = Replace
                      | Silent
                        deriving (Eq, Show)

data RundocExports = ExportCode
                   | ExportResult
                   | ExportBoth
                   | ExportNone
                     deriving (Eq, Show)

instance Default RundocExports where
  def = ExportResult

rundocOptions :: [(String, String)] -> RundocOptions
rundocOptions = foldl' parseOption def . map rmPrefix . filter isRundocOption
 where rmPrefix (k,v) = (drop (length rundocPrefix) k, v)

parseOption :: RundocOptions -> (String, String) -> RundocOptions
parseOption opts ("language", x) = opts { rundocLanguage   = parseLanguage x }
parseOption opts ("exports",  x) = opts { rundocExports    = parseExport   x }
parseOption opts ("results",  x) = opts { rundocResultType = parseResult   x }
parseOption opts _               = opts

parseLanguage :: String -> Language
parseLanguage "sh"      = Shell
parseLanguage "haskell" = Haskell
parseLanguage x         = UnknownLanguage x

parseExport :: String -> RundocExports
parseExport "result" = ExportResult
parseExport "output" = ExportResult
parseExport "code"   = ExportCode
parseExport "both"   = ExportBoth
parseExport _        = ExportNone

parseResult :: String -> RundocResultType
parseResult "silent" = Silent
parseResult _        = Replace

-- | Evaluate a code block.
evalCodeBlock :: Attr               -- ^ Old block attributes
              -> String             -- ^ Code to evaluate
              -> IO Inline          -- ^ Resulting inline
evalCodeBlock attr@(_,_,opts) code =
  let attr'  = stripRundocAttrs attr
      rdOpts = rundocOptions opts
  in
    if (Silent == rundocResultType rdOpts)
    then return $ Str ""
    else Code attr' <$> (fmap (resultString rdOpts) $ evalCode rdOpts code)

stripRundocAttrs :: Attr -> Attr
stripRundocAttrs = bimap (filter (/= rundocBlockClass))
                         (filter (not . isRundocOption))

resultString :: RundocOptions -> MetropolisResult -> String
resultString opts =
  case rundocExports opts of
    ExportCode   -> metropolisResultCode
    ExportResult -> metropolisResultOutput
    ExportBoth   -> metropolisResultCode     -- FIXME: not implemented yet
    ExportNone   -> const mempty

evalCode :: RundocOptions
         -> String
         -> IO MetropolisResult
evalCode opts code = do
  runCommand (rundocLanguage opts) code
