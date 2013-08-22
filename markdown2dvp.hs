#!/usr/bin/runhaskell -w
{-
Copyright (c) 2013 Jérémy Cochoy

This software is provided 'as-is', without any express or implied
warranty. In no event will the authors be held liable for any damages
arising from the use of this software.

Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute it
freely, subject to the following restrictions:

   1. The origin of this software must not be misrepresented; you must not
   claim that you wrote the original software. If you use this software
   in a product, an acknowledgment in the product documentation would be
   appreciated but is not required.

   2. Altered source versions must be plainly marked as such, and must not be
   misrepresented as being the original software.

   3. This notice may not be removed or altered from any source
   distribution.
-}

{-----------------------------------------------------------------------
    NOTICE : If you just want to configure the settings of this script,
             jump to BEGINING OF THE SCRIPT at the end of this file.
 -----------------------------------------------------------------------}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

-- Usefull
import           Data.String (IsString(..))
import           Data.Default (Default(..))
import           Control.Monad.State
import           Data.Maybe
import           Data.Set
-- Pandoc
import           Text.Pandoc
import           Text.Pandoc.Shared (escapeStringUsing)
-- UTF8
import           Data.Text (Text)
import qualified Data.Text as T
-- XML
import           Text.XML.Light (unode,
                                 Content(..),
                                 Element(..),
                                 ppcElement,
                                 Node,
                                 useExtraWhiteSpace,
                                 prettyConfigPP)
import qualified Text.XML.Light as XML

-- Types
type XML = [Content]

emptyXML :: XML
emptyXML = [] :: [Content]

newtype DVP = DVP String deriving (Show)

instance IsString DVP where
    fromString = DVP

dvpToString :: DVP -> String
dvpToString (DVP s) = s

type Notes = [[Block]]
type Refs = [([Inline], Target)]
data WriterState = WriterState { stNotes :: Notes
                               , stRefs  :: Refs
                               , stIds   :: [String]
                               , stPlain :: Bool
                               , stLatex :: Int}
instance Default WriterState
  where def = WriterState{ stNotes = [], stRefs = [], stIds = [],
                           stPlain = False, stLatex = 0 }

(<>) :: Node t => String -> t -> Element
(<>) = unode
infixr 8 <>

(|.) :: [XML.Attr] -> t -> ([XML.Attr], t)
(|.) args content = (args, content)
infixl 9 |.

(|=) :: String -> String -> XML.Attr
(|=) n v = XML.Attr (XML.unqual n) v

rawText str = [XML.Text $ XML.CData XML.CDataRaw str Nothing]
verbaText str = [XML.Text $ XML.CData XML.CDataVerbatim str Nothing]

class IsXML t where
  toXML :: t -> XML

instance IsXML [Char] where
  toXML str = [XML.Text $ XML.CData XML.CDataText str Nothing]

instance IsXML Element where
  toXML = (: []). Elem

instance IsXML [Element] where
  toXML = fmap Elem

instance IsXML Content where
  toXML = (: [])

instance IsXML XML where
  toXML = id

-- | Usefull function if you wan't to convert from french quote to english quote («  » vs “”)
frenchQuoteToEnglish :: String -> String
frenchQuoteToEnglish ('«' : ' ' : xs) = '“' : (frenchQuoteToEnglish xs)
frenchQuoteToEnglish (' ' : '»' : xs) = '”' : (frenchQuoteToEnglish xs)
frenchQuoteToEnglish ('«' : xs)       = '“' : (frenchQuoteToEnglish xs)
frenchQuoteToEnglish ('»' : xs)       = '”' : (frenchQuoteToEnglish xs)
frenchQuoteToEnglish (x   : xs)       = x   : (frenchQuoteToEnglish xs)
frenchQuoteToEnglish []               = []

-- Writer

-- | Convert pandoc document to a DVP xml string
writeDvp :: WriterOptions -> Pandoc -> DVP
writeDvp opts document = evalState (pandocToDvp opts document) def

-- | Take a XML tree and output a string containing xml header
renderDvp :: XML -> DVP
renderDvp = DVP . (xmlHeader ++) . display . unode "document"
  where
    -- Using extra whith space result in wrong typography.
    display = ppcElement (useExtraWhiteSpace False prettyConfigPP)
    xmlHeader = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n"

inlineListToXML :: WriterOptions -> [Inline] -> State WriterState XML
inlineListToXML opts lst = return . concat =<< mapM (inlineToXML opts) lst

inlineToXML :: WriterOptions -> Inline -> State WriterState XML

-- Worker function
warp :: IsXML a => WriterOptions -> [Inline] -> (XML -> a) -> State WriterState XML
warp w is f = do
  content <- inlineListToXML w is
  return . toXML $ f content

inlineToXML w (Emph is) = warp w is $ \content ->
  "i" <> content
inlineToXML w (Strong is) = warp w is $ \content ->
  "b" <> content
inlineToXML w (Strikeout is) = warp w is $ \content ->
  "s" <> content
inlineToXML w (Superscript is) = warp w is $ \content ->
  "sup" <> content
inlineToXML w (Subscript is) = warp w is $ \content ->
  "sub" <> content
inlineToXML w (SmallCaps is) = warp w is $ \content ->
  "span" <> ["style" |= "font-variant: small-caps;"] |. content
-- French have only « quotes ».
inlineToXML w (Quoted _ is) =  warp w is $ \content ->
  concat [rawText "&#171;&#160;", content, rawText "&#160;&#187;"]
-- Dvp inline code isn't allowed. We use <inline>, without coloration.
inlineToXML _ (Code _ str) = return . toXML $ "inline" <> str
inlineToXML _ (Str str) = return $ toXML str
inlineToXML _ (Space) = return $ toXML " "
-- Inline latex
inlineToXML (Math InlineMath str) = do

inlineToXML _ x = return . toXML . show $ x

authorToXML :: WriterOptions -> [Inline] -> State WriterState XML
authorToXML = inlineListToXML

pandocToDvp :: WriterOptions -> Pandoc -> State WriterState DVP
pandocToDvp opts (Pandoc (Meta title authors date) blocks) = do
  title' <- fmap ("titre" <>) $ inlineListToXML opts title
  authors' <- fmap concat . mapM (authorToXML opts) $ authors
  date' <- inlineListToXML opts date
  headerblock <- return . Elem $ "entete" <>
      [ "rubrique"  <> "89"
      , "meta"      <> ["description" <> "", "keywords" <> ""]
      , "titre"     <> ["page" <> title', "article" <> title']
      , "date"      <> date'
      , "miseajour" <> date'
      , "extratag"  <> emptyXML
      , "licauteur" <> maybe emptyXML toXML (listToMaybe authors')
      , "lictype"   <> "6"
      , "licannee"  <> "2013"
      , "serveur"   <> "zenol-http"
      , "chemin"    <> "relative/path/"
      , "urlhttp"   <> "http://cochoy-jeremy.developpez.com/relative/path/"
      , "pdf"       <> ["sautDePageAvantSection" <> "0",
                        "notesBasPage" <> "FinDocument"]
      ]
  authorsblock <- return . Elem $ "authorDescriptions" <> authors'
  return $ renderDvp [headerblock, authorsblock]

{- BEGINING OF THE SCRIPT -}

{- This part is the script reading MD from stdio, and outputing xml to stdout.
   The folowing line aren't licenced, and you can "Do What The Fuck you want"
   whit them -}

main :: IO ()
main = interact (dvpToString . writeDvp def . readMarkdown readerOpts)

readerOpts = def
    { readerSmart = True
    , readerExtensions = unions [pandocExtensions, multimarkdownExtensions]
    }

{- END OF THE SCRIPT -}
