module Parsec where
import Text.ParserCombinators.Parsec hiding (label)
import Control.Monad
import Ast
import SedRegex
import Prelude hiding (readFile, writeFile)
import qualified Data.ByteString.Char8 as B

import Debug.Trace
import Text.Printf

data ParserState = ParserState {
    lastRE :: Pattern
} 

type SedParser = GenParser Char ParserState
type Stream = String

eol = oneOf "\n\r" >> return ()
eoleof = choice [eol, eof]
slash = char '/'
comma = char ','
--colon = char ':'
semi = char ';'
backslash = char '\\'

emptyState :: ParserState
emptyState = ParserState { 
    lastRE = B.pack ""   
}

parseSed :: SedParser a -> Stream -> Either ParseError a
parseSed p = runParser p emptyState ""

number :: SedParser Int
number = do
    n <- many1 digit
    return $ read n

invert :: SedParser Bool
invert = (spaces >> char '!' >> return True) <|> return False

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

endCmd = choice [eol, eof, semiend, comm, spaces >> return ()]
    where
    semiend = try (spaces >> semi >> spaces >> return ())
    comm = lookAhead (char '#') >> return ()

sedCmd :: SedParser SedCmd
sedCmd = do
    a <- address
    fun <- sedFun
    return $ SedCmd a fun

group = do
    char '{'
 --   spaces
    cmds <- sedCmds
    spaces  
    char '}' <?> "}"
    return $ Group cmds
 
sedFun :: SedParser SedFun
sedFun = choice functions >>= \f -> return f

bareFun :: Char -> SedFun -> SedParser SedFun
bareFun c f = char c >> return f

comment = do
    char '#'
    manyTill anyChar (lookAhead eoleof)
    return Comment

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
--  spaces
    many $ choice[char ' ', char '\t']
    label <- manyTill anyChar (lookAhead eoleof)
    if null label then return $ f Nothing
      else return $ f (Just $ B.pack label)

transform = do
    char 'y'
    slash <?> "/"
    str1 <- manyTill anyChar slash
    str2 <- manyTill anyChar slash
    return $ Transform (B.pack str1) (B.pack str2)

unesc [] = []
unesc [x] = [x]
unesc (x:y:xs) | x:[y] == "\\t" = '\t':unesc xs
               | x:[y] == "\\n" = '\n':unesc xs
               | otherwise = x : unesc (y:xs)

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

emptyCmd = SedCmd (Address Nothing Nothing False) EmptyCmd

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

functions = 
    [substitute, group, append, change, insert, lineNum, delete, deletePat, replacePat, 
     appendPat, replaceHold, appendHold, list, next, appendLinePat, printPat,
     writeUpPat, quit, exchange, comment, branch, test, readFile, writeFile,
     label, transform
    ]


p  = parseSed sedCmds "/123/="
p1 = parseSed sedCmds "/123/{\n=\n}"
p1' = parseSed sedCmds "/123/{\n=\n}"
p2 = parseSed sedCmds "a\\\n123\\\n456"
p21 = parseSed sedCmds "a\\\n~"
p3 = parseSed sedCmds "/123/,/456/b" 
p4 = parseSed sedCmds "y/aaa/bbb/"
p5 = parseSed sedCmds  "\n"
p6 = parseSed sedCmds "s/aaa/bbb/g"
p7 = parseSed sedCmds "s'aaa'b&bb'1pw 123"
p8 = parseSed sedCmds "s/aaa/aa\1bb/g"
p9 = parseSed sedCmds "s/aaa/\3/"
p10 = parseSed sedCmds "r ReadFile.cmd"
p11 = parseSed sedCmds "w WriteFile.cmd"
p12 = parseSed sedCmds "{\n=\n=\n}"
p13 = parseSed sedCmds "=\n=\n"
p14 = parseSed sedCmds " #="
p15 = parseSed sedCmds "#n Print line before and after changes.\n{\n=\n}"
p16 = parseSed sedCmds "r ReadFile.cmd\nr ReadFile.cmd"
p17 = parseSed sedCmds "/^\\.H1/n\n/^$/d"
p18 = parseSed sedCmds "s/      /\\n/2"
p19 = parseSed sedCmds "s/Owner and Operator\\nGuide/aaa/g"
p20 = parseSed sedCmds "/Operator$/{\nN\ns/Owner and Operator\\nGuide/Installation Guide/\n}\n"
p22 = parseSed sedCmds "s/Owner and Operator Guide/Installation Guide/\n/Owner/{\nN\ns/ *\n/ /\ns/Owner andOperator Guide */Installation Guide\\n/\n}"
p23 = parseSed sedCmds "    /^\n$/D"
p24 = parseSed sedCmds "{\n{\n}\n}"
p25 = parseSed sedCmds "{\n{\nN\n}\n}"
p26 = parseSed sedCmds "/UNIX/{\n  N\n  /\nSystem/{\n  s// Operating &/\n  P\n  D\n  }\n }"
p27 = parseSed sedCmds "=\n=\n  #\n"
p28 = parseSed sedCmds "  \n =\n \n =\n"
p29 = parseSed sedCmds "  \n \n \n \n"
p30 = parseSed sedCmds "\n"

pp = parseSed sedCmds "/UNIX/N\n  /\nSystem/{\n  s// Operating &/\n  P\n  D\n  }"
pp1 = parseSed sedCmds "\n\n\n"

t  = parseSed sedCmds "{N\nN\n{=\n } \n}"
t1 = parseSed sedCmds "{\n }"
t2 = parseSed sedCmds " {\n {\n }\n a\\\n123\\\n456 \n}"
t3 = parseSed sedCmds " { { } { } { } } { }"
t4 = parseSed sedCmds "{{}{}{}}"
t5 = parseSed sedCmds "{\nN\n}\n#/Owner/{}"
t6 = parseSed sedCmds "{\n}\nP"
t7 = parseSed sedCmds "N\nP"
t8 = parseSed sedCmds "{ { } }"
t9 = parseSed sedCmds  "{\n {\n }\n = \n}"
t10 = parseSed sedCmds "{\n {\n }\n a\\\n123 \n}"


x1 = parseSed sedCmds "1!G ; $p ; h ; d ; #dflkjadslfj"
x2 = parseSed sedCmds "s///"
x3 = parseSed sedCmds "s1\1121"
x4 = parseSed sedCmds "s02030"
x5 = parseSed sedCmds "s9\1939"
x6 = parseSed sedCmds "sm2m3m"

y1 = parseSed sedCmds "1!H;1!{ x; p; x \n};d"
y2 = parseSed sedCmds "1!H;1!{x;p};d"
y3 = parseSed sedCmds "1!H ; 1!{ x ; p ; x \n } ; d #ffff"
y4 = parseSed sedCmds "/^0$/{\nc\\\nyes\n}"
y5 = parseSed sedCmds "/^0$/{\nc\\\nyes\\\nno, in\n}"

w1 = parseSed sedCmds "s/./\0 t &/"
w2 = parseSed sedCmds "s/./& t &/"
w3 = parseSed sedCmds "2,5 {\ns/[\t ]//g\n}"
w4 = parseSed sedCmds "2,5="
w5 = parseSed sedCmds "/1/,/4/ b" 

--z1 = parseSed sedCmds "h;:b\n$b\nN; /^\(.*\)\\n\1$/ { G


-- errors testing
e1 = parseSed sedCmds "a\\123\\\n456"
e2 = parseSed sedCmds "s/./\0 t &/ !aaa{}}}kkkkkkkk123,a"