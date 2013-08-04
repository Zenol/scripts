#!/usr/bin/runhaskell

--
-- Hi reader. This is a short scripts that translate latex input into
-- developpez.com xml format.
--
-- Maybe you don't know latex, maybe you are afraid of all those
-- strange symbols. But don't run away,  you can still modify a bit the
-- script to make it behave as you like. Just read the comments, find
-- the part you wan't to modify, and immite the style you'll read.
-- The compiler (if you use ghc) won't let you make something that won't work,
-- and will help you to correct the errors with meaningfull errors.
--

--
-- These do not make the cofee, sorry. You'll have to correct some peaces like
-- footnote (introduced in middle of paragraph, because the script can't easily know
-- where put the footnote).
-- Same thing for lstinline wich isn't parsed by HaTex
--

import           System.Environment
import           Control.Monad
import           Data.Maybe
import           Data.List

import           Text.LaTeX.Base.Syntax
import           Text.LaTeX.Base.Parser
import           Data.Text (Text)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Char (isSpace)

-- Latex
import           Text.LaTeX.Base.Render

-- XML
import           Text.XML.Light

-- Debug (pretty display of data)
import           Text.Groom


-- Nb : The dvp xml format don't allow UTF8,
-- but this script output uptf8 (due to the fact that
-- HaTeX accept only Text string and Text does not
-- provide utf8->latin conversion.
-- The script expect to read latin-1.
-- Once UTF8 will be supported in
-- dvp format, it'll be better. Until this time,
-- you'll have to convert your utf8 output into latin1 and
-- modify the xml header by adding 'encoding="ISO-8859-1"'.
-- For utf8 input, see utf8 comment below.

-------------------------------------
-- Convert the input to dvp format --
-------------------------------------

buildDvpHeader :: [Sugar] -> Element
buildDvpHeader s =
  unode "entete"
  [
    unode "titre"
    [
       unode "page" titleValue,
       unode "article" titleValue
    ],
    unode "auteur" authorValue
  ]
  where
    titleValue = fromMaybe "" $ getTitle s
    authorValue = fromMaybe "" $ getAuthor s

-- | List of the latex commands to keep (the one that would be treated)
goodCommands :: [String]
goodCommands = [ "caption"
               , "includegraphics"
               , "item"
               , "section"
               , "subsection"
               , "subsubsection"
               , "emph"
               , "footnote"
               , "ref"
               , "lstinline"]

-- | Replace emph and ref by Xml nodes
dvpEmphRef :: Sugar -> Sugar
dvpEmphRef (SCommand "emph" [FixArg v]) = sxmlElem [unode "i" (renderLatex v)]
dvpEmphRef (SCommand "ref" [FixArg v])  = sxmlElem [unode "b" $ "ref:" ++ (renderLatex v)]
dvpEmphRef x = x

-- | Replace SMath node by SXml nodes
dvpMath :: (String, Sugar) -> Sugar
dvpMath (n, SMath _ s) = sxmlElem [unode "latex" $ (Attr (unqual "id") n, s)]
dvpMath (_, x) = x

dvpText :: Sugar -> Sugar
dvpText (SText s) = SXml [Text . CData CDataText s $ Nothing]
dvpText x = x

concatXml :: [Sugar] -> [Sugar]
concatXml = sugarMapL' reduce
  where
    reduce (SXml a : SXml b : xs) = reduce $ SXml (a ++ b) :  xs
    reduce (x : xs) = x : reduce xs
    reduce [] = []

buildDvpSummary :: [Sugar] -> [Element]
buildDvpSummary l = (: []) . unode "synopsis" $ buildDvpSummaryAux l
  where
    buildDvpSummaryAux (l : xs) = case l of
      SEnv "abstract" args ls -> buildDvpCore ls
      _                       -> continue
      where
        continue = buildDvpSummaryAux xs
    buildDvpSummary [] = []

cutOnItem :: [Sugar] -> [[Sugar]]
cutOnItem = groupBy (\a b -> (not . isItem) a && (not . isItem) b)
  where
    isItem (SCommand "item" _) = True
    isItem _ = False

simplifySugar :: [Sugar] -> [Sugar]
simplifySugar = concatXml
                . fmap dvpMath . zip (fmap show [1..])
                . up (dvpEmphRef . dvpText)
  where
    up f = fmap (sugarMap f)

buildDvpCore :: [Sugar] -> [Element]
buildDvpCore (l : xs) = case l of
  SXml x  -> od $ unode "paragraph" x
  SCommand "footnote" [FixArg l]
          -> od $ unode "imgtext" (quickArg "type" "info", renderLatex l)
  SCommand "lstinline" _
          -> od $ unode "lstinline" ""
  SCommand "section" args
          -> od $ section "section-begin" args
  SCommand "subsection" args
          -> od $ section "subsection-begin" args
  SCommand "subsubsection" args
          -> od $ section "subsubsection-begin" args
  SEnv "itemize" _ xs
          -> od $ unode "liste" $ fmap (unode "element") . listOfNode $ xs
  SEnv "lstlisting" _ xs
          -> od $ unode "missing-code" $ sugarToElem . simplifySugar $ xs
  SEnv "figure" _ xs
          -> od $ unode "missing-figure" $ sugarToText . simplifySugar $ xs
  -- Now, they should be LineSep only beetween to paragraph, so just
  -- drop them.
  SLineSep -> continue
  SEnv "abstract" _ _ -> continue
  n       -> od $ unode "???" $ CData CDataVerbatim (show n) Nothing
  where
    od a = a : continue
    continue = buildDvpCore xs
    listOfNode :: [Sugar] -> [[Content]]
    listOfNode =  fmap sugarToElem . fmap simplifySugar . nonItem . cutOnItem
    nonItem :: [[Sugar]] -> [[Sugar]]
    nonItem ([SCommand "item" _] : xs) = nonItem xs
    nonItem (x : xs) = x : nonItem xs
    nonItem [] = []
    section n a = (unode n . concat . fmap texArgToString $ a)
    sugarToText :: [Sugar] -> [Content]
    sugarToText = fmap (\v -> Text $ CData CDataVerbatim (show v) Nothing)
    sugarToElem :: [Sugar] -> [Content]
    sugarToElem ((SXml x) : xs) = x ++ (sugarToElem xs)
    sugarToElem (x:xs) = sugarToElem xs
    sugarToElem [] = []
buildDvpCore [] = []


renderDvp :: [Element] -> String
renderDvp = (xmlHeader ++) . display . unode "document"
  where
    display = ppcElement (useExtraWhiteSpace True prettyConfigPP)
    xmlHeader = "<?xml version=\"1.0\"?>\n"

-- | True 'entry point' of the script
workOnLatex :: LaTeX -> IO ()
workOnLatex topLevel = do
  putStrLn . renderDvp $ [dvpHeader] ++ dvpSummary ++ dvpCore
--putStrLn . groom $ getDocument sugar
--putStrLn . groom $ cleanSugar sugar
  where
    dvpHeader = buildDvpHeader sugar
    dvpSummary = buildDvpSummary cleanSugar
    dvpCore   = [unode "summary" $ buildDvpCore cleanSugar]
    sugar = sugarMachine topLevel
    cleanSugar = simplifySugar . fromMaybe [] $ getDocument sugar

-- Read stdin as a tex file
-- Write stdout as a dvp xml file
main :: IO ()
main = do
  -- If your input is utf8, replace 'T.decodeLatin1' by 'T.decodeUtf8'
  latexOrError <- fmap (latexAtOnce . T.decodeLatin1) B.getContents

  case latexOrError of
    Left  error -> putStrLn $ "error: " ++ error
    Right latex -> workOnLatex latex

-------------------------
-- Work on LaTeX stuff --
-------------------------

-- | Produce a latex output from a LaTeX AST.
renderLatex :: LaTeX -> String
renderLatex = T.unpack . renderAppend . (: [])

-- | Get the LaTeX nodes from a TeXArg
extract :: TeXArg -> [LaTeX]
extract (OptArg l)   = [l]
extract (FixArg l)   = [l]
extract (MOptArg ls) = ls
extract (SymArg l)   = [l]
extract (MSymArg ls) = ls
-- | Convert a Single TeXArg node into a human readable string
texArgToString :: TeXArg -> String
texArgToString = concat . map renderLatex. filter (not . isCom) . lin . extract
  where
    isCom (TeXComment _) = True
    isCom (TeXCommS _) = True
    isCom (TeXComm _ _) = True
    isCom _ = False
    lin :: [LaTeX] -> [LaTeX]
    lin (TeXSeq l r : xs) = (lin [l]) ++ (lin [r]) ++ (lin xs)
    lin (x  : xs) = x : lin xs
    lin [] = []


---------------------------------------------------------------
-- Descript Sugar stuff and convert LaTeX AST into Sugar AST --
---------------------------------------------------------------

-- What is sugar? It's a memory representation of the input
-- suitable to be transformed in xml.
-- Why sugar? Because the 'latex' world is already used by HaTeX...

-- | This is a new clean representation of the tex AST
data Sugar =
    SText String
  | SEnv String [TeXArg] [Sugar]
  | SCommand String [TeXArg]
  | SMath MathType String
  | SLineSep
  | SComment String
    -- Xml node, used when reducing the sugar into xml :)
  | SXml [Content]
    -- Debug purpose, ignored.
  | SStrange LaTeX
    --
    deriving (Show, Eq)

-- Bad style :
instance Eq Content where
  (==) _ _ = False

sxmlElem :: [Element] -> Sugar
sxmlElem = SXml . fmap Elem

-- | Apply f on each node.
sugarMap :: (Sugar -> Sugar) -> Sugar -> Sugar
sugarMap f (SEnv s a v) = f $ SEnv s a (fmap (sugarMap f) v)
sugarMap f s = f $ s

sugarMapL :: ([Sugar] -> [Sugar]) -> Sugar -> Sugar
sugarMapL f (SEnv s a v) = SEnv s a (f $ fmap (sugarMapL f) v)
sugarMapL _ s = s

sugarMapL' :: ([Sugar] -> [Sugar]) -> [Sugar] -> [Sugar]
sugarMapL' f l = f $ fmap (sugarMapL f) l

isLineSep :: String -> Bool
isLineSep = null . dropWhile isSpace

filterQuote :: String -> String
filterQuote ('\'' : '\'' : xs) = '"' : filterQuote xs
filterQuote ('`'  : '`'  : xs) = '"' : filterQuote xs
filterQuote (x : xs) = x : filterQuote xs
filterQuote [] = []

-- | Return a SText or a SLineSep depending on the string given
sugarize :: String -> [Sugar]
sugarize s = case isLineSep s of
  True -> [SLineSep]
  False -> [SText . filterQuote . strip . rstrip $ s]

-- | Go deap in the tree and clean each Env node encountered
--   (Nb : It skip any LaTeX node in TeXArgs.)
sugarMachine :: LaTeX -> [Sugar]
sugarMachine (TeXRaw text)           = sugarize . T.unpack $ text
sugarMachine (TeXComm name args)     = [SCommand name args]
sugarMachine (TeXCommS name)         = [SCommand name []]
sugarMachine (TeXEnv name args node) = [SEnv name args (sugarMachine node)]
sugarMachine (TeXMath nType node)    = [SMath nType (renderLatex node)]
sugarMachine (TeXLineBreak _ _)      = [SLineSep]
sugarMachine l@(TeXOp _ _ _)         = [SStrange l]
sugarMachine l@(TeXBraces _)         = [SStrange l]
sugarMachine (TeXComment c)          = [SComment . T.unpack $ c]
sugarMachine (TeXSeq l r)            = (sugarMachine l) ++ (sugarMachine r)

-- | Remove all comments node from the list
stripComments :: [Sugar] -> [Sugar]
stripComments (x:xs) = case x of
  SEnv s a sugar -> SEnv s a (stripComments sugar) : stripComments xs
  SComment _     -> stripComments xs
  _              -> x : stripComments xs
stripComments [] = []

stripMedskip :: [Sugar] -> [Sugar]
stripMedskip xs = fmap (sugarMap medskipToLineSep) xs
  where
    medskipToLineSep (SCommand "medskip" _) = SLineSep
    medskipToLineSep x = x

reduceLineSepTop :: [Sugar] -> [Sugar]
reduceLineSepTop (SLineSep : SLineSep : xs) = reduceLineSep (SLineSep : xs)
reduceLineSepTop (x : xs) = x : reduceLineSep xs
reduceLineSepTop [] = []

reduceLineSep :: [Sugar] -> [Sugar]
reduceLineSep = sugarMapL' reduceLineSepTop

trimLineSepTop :: [Sugar] -> [Sugar]
trimLineSepTop = reverse . trim . reverse . trim
  where
    trim = dropWhile (SLineSep ==)

trimLineSep :: [Sugar] -> [Sugar]
trimLineSep = sugarMapL' trimLineSepTop

cleanSugar :: [Sugar] -> [Sugar]
cleanSugar = trimLineSep . reduceLineSep . stripComments . stripMedskip

-- | Keep only the command named in the string list
keepOnly :: [String] -> [Sugar] -> [Sugar]
keepOnly l (x:xs) = case x of
  SEnv s a sugar -> SEnv s a (keepOnly l sugar) : tail
  SCommand name args
    | name `elem` l -> x : tail
    | otherwise     -> tail
  _              -> x : tail
  where
    tail = keepOnly l xs
keepOnly _ [] = []

--------------------
-- Sugar commands --
--------------------

-- | Get the 'title' node from a list of nodes and extract the title
getTitle :: [Sugar] -> Maybe String
getTitle (x : xs) = case x of
  SCommand "title" [arg] -> Just . texArgToString $ arg
  _                       -> getTitle xs
getTitle [] = Nothing

-- | Get the 'author' node from a list of nodes and extract the author name
getAuthor :: [Sugar] -> Maybe String
getAuthor (x : xs) = case x of
  SCommand "author" [arg] -> Just . texArgToString $ arg
  _                       -> getAuthor xs
getAuthor [] = Nothing

-- | Find the node 'document' and return a clean list of LaTeX nodes
getDocument :: [Sugar] -> Maybe [Sugar]
getDocument (x : xs) = case x of
  SEnv "document" _ l -> Just . cleanSugar . keepOnly goodCommands $ l
  _                   -> getDocument xs
getDocument [] = Nothing



--------------------
-- Usefull struff --
--------------------

strip :: String -> String
strip = dropWhile isSpace

rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse

quickArg :: String -> String -> Attr
quickArg n v = (Attr (unqual n) v)
