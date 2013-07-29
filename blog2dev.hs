#!/usr/bin/runhaskell

import System.IO
import Data.List

main = do
  s <- fmap (work . lines) getContents
  mapM_ putStrLn s

work :: [String] -> [String]
work l = case l of
  "" : xs   -> "</paragraph>\n<paragraph>" : work xs
  x  : xs | "<pre lang=\"haskell\" colla=\"+\">" `isPrefixOf` x
            -> "</paragraph>\n<code langage=\"haskell\">\n<![CDATA[\n"
               : work ((drop (length "<pre lang=\"haskell\" colla=\"+\">") $ x) : xs)
  x  : xs | "<pre lang=\"cpp\" colla=\"+\">" `isPrefixOf` x
            -> "</paragraph>\n<code langage=\"cpp\">\n<![CDATA[\n"
               : work ((drop (length "<pre lang=\"cpp\" colla=\"+\">") $ x) : xs)
  x  : xs | "<h3>" `isPrefixOf` x && "</h3>" `isSuffixOf` x
            ->(
              "<section id=\"h3??\">\n<title>"
              ++ (take (length x - 9) . drop 4 $ x)
              ++ "</title>\n<paragraph>\n"
              )
              : work xs
  x  : xs | "<h4>" `isPrefixOf` x && "</h4>" `isSuffixOf` x
            ->(
              "<section id=\"h4??\">\n<title>"
              ++ (take (length x - 9) . drop 4 $ x)
              ++ "</title>\n<paragraph>\n"
              )
              : work xs
  x : xs | "</pre>" `isSuffixOf` x || "</Pre>" `isSuffixOf` x
            -> (
              (take (length x - 6) x) ++ "]]>\n</code>\n<paragraph>\n"
               )
              : work xs
  x  : xs   -> x : work xs
  []        -> []

