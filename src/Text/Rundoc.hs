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
                   , runBlocks
                   , runInlineCode
                   , runBlocksAndInlines
                   , rundocBlockClass
                   ) where

import           Metropolis.Types ( MetropolisResult(..)
                                  , MetropolisParameter(..)
                                  , Language(..) )
import           Metropolis.Worker (runCode)

import           Text.Pandoc ( Attr, Inline( Code, Space, Span )
                             , Block ( Para, CodeBlock, Null, Div )
                             , nullAttr )
import           Text.Pandoc.Walk (walkM)
import           Text.Pandoc.Builder (Inlines, Blocks)
import qualified Text.Pandoc.Builder as B

import           Control.Applicative ((<$>))
import           Data.Bifunctor (bimap)
import           Data.Default (Default(..))
import           Data.List (foldl', isPrefixOf)
import           Data.Maybe (fromMaybe)
import           Data.Monoid (mconcat, mempty)

-- | Run code and return the result if a rundoc code block is supplied;
-- otherwise return the argument unaltered.
rundoc :: Inline -> IO Inline
rundoc (Code attr code) | isRundocBlock attr = evalInlineCode attr code
rundoc x                                    = return x

runInlineCode :: Inline -> IO Inline
runInlineCode = rundoc

runBlocks :: Block -> IO Block
runBlocks (CodeBlock attr code) | isRundocBlock attr = evalBlock attr code
runBlocks x                                          = return x

runBlocksAndInlines :: Block -> IO Block
runBlocksAndInlines x = runBlocks x >>= walkM runInlineCode

isRundocBlock :: Attr -> Bool
isRundocBlock (_,cls,_) = rundocBlockClass `elem` cls

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
  , rundocOutfile    :: Maybe FilePath
  } deriving (Eq, Show)

instance Default RundocOptions where
  def = RundocOptions
        { rundocLanguage   = UnknownLanguage ""
        , rundocResultType = Replace
        , rundocExports    = [ExportSource]
        , rundocOutfile    = Nothing
        }

optionsToMetropolisParameters :: RundocOptions -> MetropolisParameter
optionsToMetropolisParameters opts =
  MetropolisParameter
  { parameterVariables = []
  , parameterOutfile   = rundocOutfile opts
  , parameterLanguage  = rundocLanguage opts
  }

data RundocResultType = Replace
                      | Silent
                        deriving (Eq, Show)

data Export = ExportSource
            | ExportOutput
            | ExportError
            | ExportFile
              deriving (Eq, Show)

type RundocExports = [Export]

rundocOptions :: [(String, String)] -> RundocOptions
rundocOptions = foldl' parseOption def . map rmPrefix . filter isRundocOption
 where rmPrefix (k,v) = (drop (length rundocPrefix) k, v)

parseOption :: RundocOptions -> (String, String) -> RundocOptions
parseOption opts (key, value) =
  case key of
   "language" -> opts{ rundocLanguage   = parseLanguage value }
   "exports"  -> opts{ rundocExports    = parseExport   value }
   "results"  -> opts{ rundocResultType = parseResult   value }
   "file"     -> opts{ rundocOutfile    = Just value }
   _          -> opts

parseLanguage :: String -> Language
parseLanguage lang =
  case lang of
    "sh"      -> Shell
    "haskell" -> Haskell
    "ditaa"   -> Ditaa
    "R"       -> R
    x         -> UnknownLanguage x

parseExport :: String -> RundocExports
parseExport "result" = [ExportOutput]
parseExport "output" = [ExportOutput]
parseExport "code"   = [ExportSource]
parseExport "both"   = [ExportSource, ExportOutput]
parseExport "file"   = [ExportFile]
parseExport _        = []

parseResult :: String -> RundocResultType
parseResult "silent" = Silent
parseResult _        = Replace

-- | Evaluate a code block.
evalInlineCode :: Attr               -- ^ Old block attributes
               -> String             -- ^ Code to evaluate
               -> IO Inline          -- ^ Resulting inline
evalInlineCode attr@(_,_,opts) code =
  let attr'  = stripRundocAttrs attr
      rdOpts = rundocOptions opts
  in (setCodeAttr attr' . fromInlines . resultToInlines rdOpts)
       <$> evalCode rdOpts code

evalBlock :: Attr
          -> String
          -> IO Block
evalBlock attr@(_,_,opts) code =
  let attr'  = stripRundocAttrs attr
      rdOpts = rundocOptions opts
  in (setBlockCodeAttr attr' . fromBlocks . inlinesToBlocks . resultToInlines rdOpts)
       <$> evalCode rdOpts code

setCodeAttr :: Attr -> Inline -> Inline
setCodeAttr attr x =
  case x of
   Code _ code -> Code attr code
   _           -> x

setBlockCodeAttr :: Attr -> Block -> Block
setBlockCodeAttr attr x =
  case x of
   CodeBlock _ code -> CodeBlock attr code
   _                -> x

stripRundocAttrs :: Attr -> Attr
stripRundocAttrs = bimap (filter (/= rundocBlockClass))
                         (filter (not . isRundocOption))

evalCode :: RundocOptions
         -> String
         -> IO MetropolisResult
evalCode opts = runCode (optionsToMetropolisParameters opts)

resultToInlines :: RundocOptions -> MetropolisResult -> Inlines
resultToInlines opts res =
  if Silent == rundocResultType opts
  then mempty
  else mconcat . map (`exporter` res) $ rundocExports opts

fromInlines :: Inlines -> Inline
fromInlines inlines =
  case B.toList inlines of
    []     -> Space
    (x:[]) -> x
    xs     -> Span nullAttr xs

inlinesToBlocks :: Inlines -> Blocks
inlinesToBlocks = fmap inlineToBlock

inlineToBlock :: Inline -> Block
inlineToBlock = Para . (:[])

fromBlocks :: Blocks -> Block
fromBlocks blocks =
  case B.toList blocks of
    []     -> Null
    (x:[]) -> x
    xs     -> Div nullAttr xs

exporter :: Export -> MetropolisResult -> Inlines
exporter e =
  case e of
    ExportSource -> exportSource
    ExportOutput -> exportOutput
    ExportFile   -> exportFile
    _            -> error "Not implemented yet"

exportOutput :: MetropolisResult -> Inlines
exportOutput = B.codeWith nullAttr .  resultOutput

exportSource :: MetropolisResult -> Inlines
exportSource = B.codeWith nullAttr . resultSource

exportFile :: MetropolisResult -> Inlines
exportFile = (\url -> B.image url "" mempty) . fromMaybe "" . resultFile
