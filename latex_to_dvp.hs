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

import           System.Environment
import           Control.Monad

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


-- Nb : The dvp xml format don't allow UTF8.
-- Due to that, and the fact that HaTex _need_ UTF8 strings,
-- I'm expecting to read latin-1, and produce latin-1,
-- though some "ugly" cheats. Once UTF8 will be supported in
-- dvp format, I'll be able to remove that. Until this time,
-- you'll have to stick to latin-1 output and modify this file
-- for utf8-input. (See utf8 comment below)

-------------------------------------
-- Convert the input to dvp format --
-------------------------------------

buildDvpHeader :: [Sugar] -> Element
buildDvpHeader s = unode "test" "hello"

-- | True 'entry point' of the script
workOnLatex :: LaTeX -> IO ()
workOnLatex topLevel = do
  putStrLn . ("Author: "++) . show . getAuthor $ sugar
  putStrLn . ("Title: "++)  . show . getTitle  $ sugar
  putStrLn . ppTopElement $ buildDvpHeader sugar
  putStrLn . groom $ cleanSugar sugar
  putStrLn . groom $ getDocument sugar
  where
    sugar = sugarMachine topLevel

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
texArgToString = concat . map renderLatex . extract

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
    -- Debug purpose, ignored.
  | SStrange LaTeX
    deriving (Show, Eq)

-- | Apply f on each node.
sugarMap :: (Sugar -> Sugar) -> Sugar -> Sugar
sugarMap f (SEnv s a v) = f $ SEnv s a (fmap (sugarMap f) v)
sugarMap f s = f $ s

sugarMapL :: ([Sugar] -> [Sugar]) -> Sugar -> Sugar
sugarMapL f (SEnv s a v) = SEnv s a (fmap (sugarMapL f) v)
sugarMapL _ s = s

sugarMapL' :: ([Sugar] -> [Sugar]) -> [Sugar] -> [Sugar]
sugarMapL' f l = f $ fmap (sugarMapL f) l

isLineSep :: String -> Bool
isLineSep = null . dropWhile isSpace

-- | Return a SText or a SLineSep depending on the string given
sugarize :: String -> [Sugar]
sugarize s = case isLineSep s of
  True -> [SLineSep]
  False -> [SText . strip . rstrip $ s]

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

cleanSugar :: [Sugar] -> [Sugar]
cleanSugar = reduceLineSep . stripComments . stripMedskip
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
  SEnv "document" _ l -> Just l
  _                   -> getDocument xs
getDocument [] = Nothing



--------------------
-- Usefull struff --
--------------------

strip :: String -> String
strip = dropWhile isSpace

rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse

