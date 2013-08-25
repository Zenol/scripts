#!/usr/bin/runhaskell
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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{- This part is the script reading MD from stdio, and outputing xml to stdout.
   The folowing line aren't licenced, and you can "Do What The Fuck you want"
   whit them -}

-- App
import           System.IO
import           Debug.Trace
import           Data.Text.ICU.Convert as ICU
import qualified Data.Text.IO as TIO
import qualified Data.Set as Set
import           Text.Pandoc
import           Text.Pandoc.Writers.Dvp
-- UTF8
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B
-- Usefull
import           Control.Monad
import           Data.Default

main :: IO ()
main = do
  s <- T.unpack `liftM` TIO.getContents
  o <- return . writeDvp' (def) . readMarkdown readerOpts $ s
  mapM_ (hPutStrLn stderr) $ snd o
  B.putStrLn . B.pack . dvpToString $ fst o

writerNoFootnote s = s { writerExtensions = writerExtensions s Set.\\ Set.fromList [Ext_footnotes]}

readerOpts = def
    { readerSmart = True
    , readerExtensions = Set.unions
        [ pandocExtensions, multimarkdownExtensions] Set.\\
        Set.fromList [Ext_raw_html]
    }
