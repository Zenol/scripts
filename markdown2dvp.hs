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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- App
import Debug.Trace
import           Data.Text.ICU.Convert as ICU
import qualified Data.Text.IO as TIO

-- Usefull
import           Data.String (IsString(..))
import           Data.Default (Default(..))
import           Control.Monad.State
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Char (ord, toLower)
import           Control.Applicative (Alternative(..), (<|>), (*>), (<$), (<$>))
import           Data.List
import           Text.Numeral.Roman
import           Numeric (showIntAtBase)
-- App
import           System.IO
-- Pandoc
import           Text.Pandoc
import           Text.Pandoc.Shared (escapeStringUsing)
-- UTF8
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B
-- XML
import           Text.XML.Light (unode,
                                 Content(..),
                                 Element(..),
                                 ppcElement,
                                 showContent,
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

-- | Zipper that focus the current section level
data SecNumZipper = SecNumZipper { snzCurrentSection :: Int
                                 , snzParentSections :: [Int]
                                 }

instance Default SecNumZipper where
  def = SecNumZipper { snzCurrentSection = 1
                     , snzParentSections = []
                     }

-- | Go one section up (i.e. forget about the current sub section
--   and get back to parrent).
snzUp :: SecNumZipper -> SecNumZipper
snzUp snz@SecNumZipper{..} = case snzParentSections of
  [] -> error "Tryed to go up in the SecNumZipper, but snzParentSections is empty!"
  (x : xs) -> snz {snzCurrentSection = x, snzParentSections = xs}

-- | Go to the next section (increment the section number).
snzInc :: SecNumZipper -> SecNumZipper
snzInc snz = snz {snzCurrentSection = 1 + snzCurrentSection snz}

-- | Do one section up (i.e. create a new sub section inside the
--   current section).
snzDown :: SecNumZipper -> SecNumZipper
snzDown SecNumZipper{..} = SecNumZipper 1 (snzCurrentSection : snzParentSections)

modifySnz :: (SecNumZipper -> SecNumZipper) -> State WriterState ()
modifySnz f = modify (\s -> s {stSecNum = f $ stSecNum s})

type Notes = [[Block]]
type Refs = [([Inline], Target)]
type SectionHeader = (Int, String, XML)
data WriterState = WriterState { stNotes        :: Notes
                               , stNoXML        :: Bool
                               , stRefs         :: Refs
                               , stSecNum       :: SecNumZipper
                                 -- Unused, just in case we implement
                                 -- citations.
                               , stLatex        :: Int
                               , stPlain        :: Bool
                               , stLogs         :: [String]}

instance Default WriterState where
  def = WriterState { stNotes = []
                    , stNoXML = False
                    , stRefs = []
                    , stSecNum = def
                    , stLatex = 0
                    , stLogs = []
                    , stPlain = False
                    }
incrementLatex :: State WriterState ()
incrementLatex = modify (\s -> s {stLatex = 1 + stLatex s})

getSectionId :: State WriterState String
getSectionId = do
  snz <- gets stSecNum
  let idList = (reverse $ snzCurrentSection snz : snzParentSections snz)
  return . intercalate "." . map getnum $ zip [0..] idList
  where
    getnum (a, b) = case a of
      0 -> toRoman b -- Roman
      1 -> toAlpha b -- Alpha
      _ -> show b    -- Numeric

plainMode :: Bool -> State WriterState Bool
plainMode mode = do
  oldMode <- gets stPlain
  modify (\s -> s {stPlain = mode})
  return oldMode

noXMLMode :: Bool -> State WriterState Bool
noXMLMode mode = do
  oldMode <- gets stNoXML
  modify (\s -> s {stNoXML = mode})
  return oldMode

writeLog :: String -> State WriterState ()
writeLog msg = modify (\s -> s { stLogs = msg : stLogs s })

toAlpha :: Int -> String
toAlpha m = concat [replicateM n ['A'..'Z'] | n <- [0..]] !! m

(<>) :: Node t => String -> t -> Element
(<>) = unode
infixr 8 <>

(<^>) :: Node t => String -> t -> Content
(<^>) =  fmap (fmap Elem) unode
infixr 8 <^>

(<!>) :: Node t => String -> t -> XML
(<!>) =  fmap (fmap toXML) unode
infixr 8 <!>

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

maybeDo :: (a -> b) -> Maybe a -> [b]
maybeDo f v = maybeToList $ fmap f v

-- | Usefull function if you wan't to convert from french quote
--   to english quote («  » vs “”).
frenchQuoteToEnglish :: String -> String
frenchQuoteToEnglish ('«' : ' ' : xs) = '“' : (frenchQuoteToEnglish xs)
frenchQuoteToEnglish (' ' : '»' : xs) = '”' : (frenchQuoteToEnglish xs)
frenchQuoteToEnglish ('«' : xs)       = '“' : (frenchQuoteToEnglish xs)
frenchQuoteToEnglish ('»' : xs)       = '”' : (frenchQuoteToEnglish xs)
frenchQuoteToEnglish (x   : xs)       = x   : (frenchQuoteToEnglish xs)
frenchQuoteToEnglish []               = []

displayListStyle :: ListNumberStyle -> String
displayListStyle DefaultStyle = "1"
displayListStyle Example = "1"
displayListStyle Decimal = "1"
displayListStyle LowerRoman = "i"
displayListStyle UpperRoman = "I"
displayListStyle LowerAlpha = "a"
displayListStyle UpperAlpha = "A"

showAlpha :: Int -> String
showAlpha n = showIntAtBase 26 (\n -> ['A' .. 'Z'] !! n) n ""

-- Writer


-- | Convert pandoc document to a DVP xml string, hidding warnings.
writeDvp :: WriterOptions -> Pandoc -> String
writeDvp opts document = dvpToString . (\(a, _) -> a) $ writeDvp' opts document

-- | Convert pandoc document to a DVP xml string
writeDvp' :: WriterOptions -> Pandoc -> (DVP, [String])
writeDvp' opts document = extract $ runState (pandocToDvp opts document) def
  where
    extract (a, b) = (a, stLogs b)

-- | Take a XML tree and output a string containing xml header
renderDvp :: XML -> DVP
renderDvp = DVP . (xmlHeader ++) . display . unode "document"
  where
    -- Using extra whith space may result in typo. or unreadable output.
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

blockListToXML :: WriterOptions -> [Block] -> State WriterState XML
blockListToXML w xs = do
  blocks <- mapM (blockToXML w) xs
  sectionify blocks
  where
    -- A well structured document doesnt contain stuff outside a section.
    -- But not all documents are well structured :) Anyway, the dvp kit is
    -- permissive enougth to build even if the XML isn't valid.
    sectionify :: [Either XML SectionHeader] -> State WriterState XML
    sectionify [] = return []
    sectionify ((Right (level, id, title)) : xs) = do
      let (subtree, neighboor) = span (belongToSubtree level) xs
      sectionId <- getSectionId -- We get the current section ID
      modifySnz snzDown         -- We compute sub sections so let's go down
      xmlSubtree <- sectionify subtree
      modifySnz snzUp           -- Go up, end of subsections
      modifySnz snzInc          -- What happen next belong to the next section
      xmlNeighboor <- sectionify neighboor
      return $ (:)
        ("section" <^> ["id" |= sectionId] |. (("title" <^> title) : xmlSubtree))
        (xmlNeighboor)
    sectionify ((Left x) : xs) = (x++) `fmap` sectionify xs

    belongToSubtree :: Int -> Either XML SectionHeader -> Bool
    belongToSubtree _ (Left _) = True
    belongToSubtree n (Right (level, _, _)) = level > n


paragraph :: XML -> Either XML SectionHeader
paragraph = Left . ("paragraph" <!>)

xmlRet :: (Monad m, IsXML t) => t -> m (Either XML a)
xmlRet = return . Left . toXML

blockToXML :: WriterOptions -> Block -> State WriterState (Either XML SectionHeader)
blockToXML w  (Para is) = work =<< gets stPlain
  where
    work st = case st of
      False -> paragraph <$> warp w is id
      True  -> (Left . inlineParagraph) <$> warp w is id
    inlineParagraph xml = concat [xml, "br" <!> emptyXML, "br" <!> emptyXML]
blockToXML w (Para [Image alt (src, 'f':'i':'g':':':title)]) =
  blockToXML w (Para [Image alt (src, title)])
blockToXML w  (Para [Image is (url, title)]) = do
  alt <- liftM (concat . map showContent) $ inlineListToXML w is
  xmlRet $ "image" <!> ["src" |= url, "titre" |= title, "alt" |= alt] |. emptyXML
blockToXML w  (Plain is) = do
  mode <- plainMode True
  v <- warp w is id
  plainMode mode
  xmlRet $ v
blockToXML w  (CodeBlock (id, classes, xs) code ) = xmlRet $
  "code" <!> args |. (verbaText code)
  where
    args = concat [ maybeDo ("langage" |=) $
       (listToMaybe . filter ("numberLines" /=) $ classes)
                  , maybeDo ("titre" |=) (lookup "titre" xs)
                  , maybeDo ("startLine" |=) $
       (lookup "startFrom" xs) <|> ("1" <$ find ("numberLines"==) classes)
                  , maybeDo ("showLines" |=) $
       (lookup "showLines" xs
        <|> lookup "startFrom" xs
        <|> "1" <$ find ("numberLines"==) classes) *> Just "1"
                  ]
blockToXML w (Header level (id, _, _) is) = do
  title <- inlineListToXML w is
  return $ Right (level, id, title)
-- TODO : Find a nice way to handle BlockQuote inside BlockQuote
blockToXML w (BlockQuote blocks) = do
  mode <- plainMode True
  quote <- liftM concat $ mapM (clean <=< blockToXML w) blocks
  plainMode mode
  xmlRet $ "citation" <!> quote
    where
      clean (Left x) = return x
      clean (Right _) = return emptyXML
blockToXML w debug@(OrderedList (start, style, delim) blocks) = do
  xmls <- mapM (blockListToXML w) blocks
  xmlRet $ "liste" <!> args |.
    map (\s -> "element" <^> ["useText" |= "0"] |. s) xmls
  where
    args = concat [ ["type" |= displayListStyle style]
                  , maybeDo ("start" |=) $ case start of
                       1 -> Nothing
                       s -> Just $ show s
                  ]
blockToXML w debug@(BulletList blocks) = do
  xmls <- mapM (blockListToXML w) blocks
  xmlRet $ "liste" <!> map makeListRoot xmls
blockToXML w (DefinitionList namedBlocks) = do
  xmls <- itemify <$> xmlify namedBlocks
  xmlRet $ "liste" <!> xmls
  where
    applyInTuple :: ([Inline], [[Block]]) -> State WriterState (XML, [XML])
    applyInTuple (a, b) = do
      b' <- mapM (blockListToXML w) b
      a' <- inlineListToXML w a
      return (a', b')
    xmlify :: [([Inline], [[Block]])] -> State WriterState [(XML, [XML])]
    xmlify = mapM applyInTuple
    itemify :: [(XML, [XML])] -> XML
    itemify xmls = "liste" <!>
      [makeListRoot $ "paragraph" <!> "b" <!> a
                   ++ "liste" <!> map makeListRoot b
      | (a, b) <- xmls]
blockToXML w (HorizontalRule) = do
  xmlRet $ "html-brut" <!> verbaText "<hr />"
blockToXML w table@(Table caption aligns rcWs headers rows) = do
  writeLog $ "Table not yet implemented: " ++ (show table)
  caption' <- liftM (concat . fmap showContent) $ inlineListToXML w caption
  titles <- liftM concat . mapM cellify $ zip3 aligns rcWs headers
  lines <- mapM linify $ map (zip3 aligns rcWs) rows
  xmlRet $ "tableau" <!> ["border" |= "1", "legende" |= caption'] |.
    ("entete" <!> titles ++ concat lines)
  where
    getAlign AlignLeft = Just "left"
    getAlign AlignRight = Just "right"
    getAlign AlignCenter = Just "center"
    getAlign AlignDefault = Nothing
    width rcW = show (truncate $ 100 * rcW)
    linify :: [(Alignment, Double, [Block])] -> State WriterState XML
    linify cells = do
      xmls <- mapM cellify $ cells
      return $ "ligne" <!> (concat xmls)
    cellify (align, rcW, block) = do
      xmls <- blockListToXML w block
      return . ("colonne" <!>) $
        concat [ ["useText" |= "0"]
               , maybeDo ("align" |=) (getAlign align)
               , if (rcW /= 0) then ["width" |= (width rcW ++ "%")] else []
               ] |. xmls
blockToXML w block = do
  writeLog $ "Unknown block: " ++ (show block)
  xmlRet $ emptyXML

makeListRoot s = "element" <^> ["useText" |= "0"] |. s
makeListRoot s = "element" <^> ["useText" |= "0"] |. s

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
inlineToXML _ (Code _ str) = return $ "inline" <!> str
inlineToXML _ (Str str) = return . toXML $ str
inlineToXML _ (Space) = return $ toXML " "
-- Inline latex
inlineToXML w (Math InlineMath str) = do
  id <- fmap (("latex-" ++) . show) $ gets stLatex
  incrementLatex
  return $ "latex" <!> ["id" |= id] |. (verbaText str)
-- Raw stuff isn't supported
inlineToXML w (RawInline f str) = do
  writeLog $ "RawInline not supported: " ++ f ++ " - " ++ str
  return . toXML $ str
inlineToXML w c@(Cite _ is) = do
  writeLog $ "Citation not supported: " ++ (show c)
  warp w is id
inlineToXML w (Link is (url, "")) = do
  content <- inlineListToXML w is
  return $ "link" <!> ["href" |= url] |. content
inlineToXML w (Link is (url, title)) = do
  content <- inlineListToXML w is
  return $ "link" <!> ["href" |= url, "title" |= title] |. content
inlineToXML w (Image is (url, title)) = warp w is $ \content ->
  let alt = concat $ map showContent content in
  "image" <!> ["src" |= url, "titre" |= title, "alt" |= alt] |. emptyXML
inlineToXML w (Note bs) = case isEnabled Ext_footnotes w of
    True -> do
      modify (\st -> st{ stNotes = bs : stNotes st })
      ref <- (show . length) `fmap` gets stNotes
      return $ "renvoi" <!> ["id" |= ref] |. (concat $ toXML `map` ["[", ref, "]"])
    False -> do
      content <- blockListToXML w $ bs
      return . concat $ [toXML "[", content , toXML "]"]

inlineToXML _ x = return . toXML . show $ x

authorToXML :: WriterOptions -> [Inline] -> State WriterState XML
authorToXML = inlineListToXML

pandocToDvp :: WriterOptions -> Pandoc -> State WriterState DVP
pandocToDvp w (Pandoc (Meta title authors date) blocks) = do
--  trace (show blocks) $ return ()
  title' <- inlineListToXML w title
  page' <- inlineListToXML w title
  authors' <- fmap concat . mapM (authorToXML w) $ authors
  date' <- inlineListToXML w date
  headerblock <- return $ "entete" <!>
      [ "rubrique"  <> "89"
      , "meta"      <> ["description" <> "", "keywords" <> ""]
      , "titre"     <> ["page" <> page', "article" <> title']
      , "date"      <> date'
      , "miseajour" <> date'
      , "extratag"  <> emptyXML
      , "licauteur" <> maybe emptyXML toXML (listToMaybe authors')
      , "lictype"   <> "6"
      , "licannee"  <> "2013"
      , "serveur"   <> "zenol-http"
      , "chemin"    <> "relative/path"
      , "urlhttp"   <> "http://cochoy-jeremy.developpez.com/relative/path/"
      , "pdf"       <> ["sautDePageAvantSection" <> "0",
                        "notesBasPage" <> "FinDocument"]
      ]
  authorsblock <- return $ "authorDescriptions" <!> authors'
  content <- blockListToXML w blocks
  foot <- case isEnabled Ext_footnotes w of
    False -> return $ emptyXML
    True  -> (paragify . listify . refify) =<< xmlify =<< gets stNotes
  return . renderDvp . concat $ [ headerblock
                                , authorsblock
                                , "summary" <!> (content ++ foot)
                                ]
  where
    paragify :: XML -> State WriterState XML
    paragify xml = do
      sectionId <- getSectionId
       -- Since this function is the last building a section, we don't need:
      modifySnz snzInc
      return $ "section" <!> ["id" |= sectionId] |. (("title" <!> "Références") ++ xml)
    listify :: [XML] -> XML
    listify xmls = "liste" <!> ["type" |= "1"] |.
      map (\s -> "element" <^> ["useText" |= "0"] |. s) xmls
    refify :: [XML] -> [XML]
    refify l = map
                 (\(n, xml) -> ("signet" <^> ["id" |= (show n)] |. emptyXML) : xml)
                 (zip [1..] l)
    xmlify :: [[Block]] -> State WriterState [XML]
    xmlify = mapM (blockListToXML w)

{- BEGINING OF THE SCRIPT -}

{- This part is the script reading MD from stdio, and outputing xml to stdout.
   The folowing line aren't licenced, and you can "Do What The Fuck you want"
   whit them -}

main :: IO ()
main = do
  s <- T.unpack `liftM` TIO.getContents
  o <- return . writeDvp' (def) . readMarkdown readerOpts $ s
  mapM_ (hPutStrLn stderr) $ snd o
  B.putStrLn . B.pack . escape . dvpToString $ fst o
  where
    -- This is a big hack, because escape is piped after the XML processing,
    -- and <![CDATA[ fields will be affected. But, at least, it output
    -- wellformed XML since none of the markup use non-latin1 characters.
    escape :: String -> String
    escape s = s >>= \c -> if c < '\x007f'
                           then [c]
                           else "&#" ++ show (ord c) ++ ";"

writerNoFootnote s = s { writerExtensions = writerExtensions s Set.\\ Set.fromList [Ext_footnotes]}

readerOpts = def
    { readerSmart = True
    , readerExtensions = Set.unions
        [ pandocExtensions, multimarkdownExtensions] Set.\\
        Set.fromList [Ext_raw_html]
    }

{- END OF THE SCRIPT -}
