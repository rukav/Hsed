-- |
-- Module      :  Parsec
-- Copyright   :  (c) Vitaliy Rkavishnikov
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  virukav@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Sed commands parser. See "The Open Group Base Specifications Issue 7" for
-- parsing requirements. The current version of the Haskell Sed doesn't supports
-- the back-references in the RE.

module Parsec where

import Prelude hiding (readFile, writeFile)
import Text.ParserCombinators.Parsec hiding (label)
import Text.Printf
import qualified Data.ByteString.Char8 as B
import Ast
import SedRegex

-- | If an RE is empty last RE used in the last command applied 
data ParserState = ParserState {
    lastRE :: Pattern
} 

emptyState = ParserState { 
    lastRE = B.pack ""   
}

type SedParser = GenParser Char ParserState
type Stream = String

eol = oneOf "\n\r" >> return ()
eoleof = choice [eol, eof]
slash = char '/'
comma = char ','
semi = char ';'
backslash = char '\\'
number = many1 digit >>= \n -> return $ read n
invert = (spaces >> char '!' >> return True) <|> return False

parseSed :: SedParser a -> Stream -> Either ParseError a
parseSed p = runParser p emptyState ""

parseRE :: String -> SedParser Pattern
parseRE pat = do
    let patB = B.pack pat
    updateState (\(ParserState re)  -> ParserState patB)
    return patB

pattern open close val = do
    pat <- between open close val
    if null pat then do
        s <- getState
        return $ lastRE s
     else parseRE (unesc pat)

addr = 
    fmap (Just . LineNumber) number <|>
    (char '$' >> return (Just LastLine)) <|>
    (pattern slash slash val >>= \pat -> return $ Just (Pat pat))
    where val = many (noneOf "/")

addr1 = do
    a1 <- addr
    spaces
    b <- invert
    return $ Address a1 Nothing b

addr2 = do
    a1 <- addr
    comma <?> ","
    a2 <- addr <?> "bad address" 
    spaces
    b <- invert
    return $ Address a1 a2 b

address :: SedParser Address
address = 
    try addr2 <|> try addr1 <|> 
    (invert >>= \b -> return $ Address Nothing Nothing b) 

sedCmds :: SedParser [SedCmd]
sedCmds = 
    many1 $ try (space >> return emptyCmd) <|>
            (do { x <- sedCmd; endCmd; return x })
    where
    endCmd = choice [eol, eof, semiend, comm, spaces >> return ()]
       where
       semiend = try (spaces >> semi >> spaces >> return ())
       comm = lookAhead (char '#') >> return ()

sedCmd :: SedParser SedCmd
sedCmd = do
    a <- address
    fun <- sedFun
    return $ SedCmd a fun

sedFun :: SedParser SedFun
sedFun = choice functions >>= \f -> return f

functions = 
    [substitute, group, append, change, insert, lineNum, delete, deletePat, replacePat, 
     appendPat, replaceHold, appendHold, list, next, appendLinePat, printPat,
     writeUpPat, quit, exchange, comment, branch, test, readFile, writeFile,
     label, transform
    ]

append        = textFun 'a' Append
change        = textFun 'c' Change 
insert        = textFun 'i' Insert

readFile      = fileFun 'r' ReadFile
writeFile     = fileFun 'w' WriteFile
label         = argFun ':' Label

lineNum       = bareFun '=' LineNum
delete        = bareFun 'd' Delete
deletePat     = bareFun 'D' DeletePat
replacePat    = bareFun 'g' ReplacePat
appendPat     = bareFun 'G' AppendPat
replaceHold   = bareFun 'h' ReplaceHold
appendHold    = bareFun 'H' AppendHold
list          = bareFun 'l' List
next          = bareFun 'n' Next
appendLinePat = bareFun 'N' AppendLinePat
printPat      = bareFun 'p' PrintPat
writeUpPat    = bareFun 'P' WriteUpPat
quit          = bareFun 'q' Quit
exchange      = bareFun 'x' Exchange

branch        = gotoFun 'b' Branch
test          = gotoFun 't' Test

bareFun :: Char -> SedFun -> SedParser SedFun
bareFun c f = char c >> return f

textFun :: Char -> (Text -> SedFun) -> SedParser SedFun
textFun c f = do
    char c
    backslash <?> "backslash"
    eol <?> "end of line"
    parts <- lines
    return $ f (B.pack (init $ unlines parts))
    where
       lines = do {x <- line; try eoleof; return x}
       line = sepBy part (backslash >> eol)
       part = many (noneOf "\\\n")

fileFun :: Char -> (FilePath -> SedFun) -> SedParser SedFun
fileFun c f = 
    char c >> spaces >> 
    manyTill anyChar (lookAhead eoleof) >>= \l -> return $ f l

argFun :: Char -> (B.ByteString -> SedFun) -> SedParser SedFun
argFun c f = 
    char c >> spaces >> 
    manyTill anyChar (lookAhead eoleof) >>= \l -> return $ f (B.pack l)

gotoFun :: Char -> (Maybe Label -> SedFun) -> SedParser SedFun
gotoFun c f = do
    char c
    many $ choice[char ' ', char '\t']
    label <- manyTill anyChar (lookAhead eoleof)
    if null label then return $ f Nothing
      else return $ f (Just $ B.pack label)

group = do
    char '{'
    cmds <- sedCmds
    spaces  
    char '}' <?> "}"
    return $ Group cmds

comment = do
    char '#'
    manyTill anyChar (lookAhead eoleof)
    return Comment

transform = do
    char 'y'
    slash <?> "/"
    str1 <- manyTill anyChar slash
    str2 <- manyTill anyChar slash
    return $ Transform (B.pack str1) (B.pack str2)

substitute = do
    char 's'
    delim <- lookAhead anyChar
    let val = many $ noneOf [delim]
    pat <- pattern (char delim) (char delim) val
    repl <- rhs delim
    fs <- flags 
    return $ Substitute (B.pack $ unesc (B.unpack pat)) (B.pack $ esc repl) fs
    where
      esc [] = []
      esc [x] | x == '&' = "\\0"
              | otherwise = [x]
      esc (x:y:ys) | [x,y] == "\\n" = '\n':esc ys
                   | [x,y] == "\\\n" = esc (y:ys)
                   | [x,y] == "\\&" = '&':esc ys
                   | x     == '&' = "\\0" ++ esc (y:ys)
                   | otherwise = x:esc(y:ys)           

      rhs delim = manyTill anyChar (char delim)

flags = do
    op <- occur
    out <- outFile
    return $ Flags op out
    where
      occur = occurPrint <|> return Nothing
      outFile = 
          (char 'w' >> spaces >> 
          manyTill anyChar (lookAhead eoleof) >>= \f -> return $ Just f) <|>
          return Nothing
      occurPrint = 
          occurrence >>= \o -> prn >>= \p ->
          return $ Just $ OccurrencePrint o p
      occurrence = 
          (char 'g' >> return (Just ReplaceAll)) <|>
          (number >>= \n -> return $ Just $ Replace n) <|>
          return Nothing
      prn = 
          (char 'p' >> return True) <|>
          return False

unesc [] = []
unesc [x] = [x]
unesc (x:y:xs) | x:[y] == "\\t" = '\t':unesc xs
               | x:[y] == "\\n" = '\n':unesc xs
               | otherwise = x : unesc (y:xs)

emptyCmd = SedCmd (Address Nothing Nothing False) EmptyCmd
