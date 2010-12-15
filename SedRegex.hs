module SedRegex where

import Data.Array((!))
import Data.Bits((.|.))
import Text.Regex.Base(RegexMaker(makeRegexOpts),defaultExecOpt,RegexLike(matchAll,matchAllText),RegexContext(matchM),MatchText)
import Text.Regex.Posix
import Text.Regex.Base.RegexLike
import Text.Regex
import Debug.Trace

data SedPattern = SedPattern Circumflex Pattern Dollar deriving Show
type Pattern = String
data RangeChars = RangeChars (Maybe Char) (Maybe Char)  
                | String String                   
    deriving Show

data BracketExp = CollatingElem String 
                | CollatingSym String 
                | EquivClass String 
                | CharClass RangeChars
    deriving Show

type Negate = Bool
type Index = Int
type Low = Maybe Int
type High = Maybe Int
type Circumflex = Maybe Char                      
type Dollar = Maybe Char                          

sedSubRegex :: String                         -- ^ Search pattern
            -> String                         -- ^ Input string
            -> String                         -- ^ Replacement text
            -> Int                            -- ^ Occurrence
            -> (String, Bool)                 -- ^ (Output string, Replacement occurs)
--sedSubRegex _ "" _ _ = ("", True)
sedSubRegex pat inp repl n =
  let regexp = makeRegexOpts compExtended defaultExecOpt pat 
      compile _i str [] = \ _m ->  (str++)
      compile i str (("\\",(off,len)):rest) =
        let i' = off+len
            pre = take (off-i) str
            str' = drop (i'-i) str
        in if null str' then \ _m -> (pre ++) . ('\\':)
             else \  m -> (pre ++) . ('\\' :) . compile i' str' rest m
      compile i str ((xstr,(off,len)):rest) =
        let i' = off+len
            pre = take (off-i) str
            str' = drop (i'-i) str
            x = read xstr
        in if null str' then \ m -> (pre++) . (fst (m!x) ++)
             else \ m -> (pre++) . (fst (m!x) ++) . compile i' str' rest m
      compiled :: MatchText String -> String -> String
      compiled = compile 0 repl findrefs where
        -- bre matches a backslash then capture either a backslash or some digits
        bre = mkRegex "\\\\(\\\\|[0-9]+)"
        findrefs = map (\m -> (fst (m!1),snd (m!0))) (matchAllText bre repl)
      go _i str [] = str
      go i str (m:ms) =
        let (_,(off,len)) = m!0
            i' = off+len
            pre = take (off-i) str
            str' = drop (i'-i) str
        in if null str' then pre ++ compiled m ""
             else pre ++ compiled m (go i' str' ms)
      occur _ "" _ _ _ = ("", False)
      occur regexp inp repl n pre =
        case matchOnceText regexp inp of
           Nothing -> (inp, False)
           Just (p, m, r) -> 
              if n > 1 then 
                 let acc = pre ++ p ++ fst (m!0) in
                     occur regexp r repl (n-1) acc
               else
                  (pre ++ go 0 inp [m], True)
  in if n == 0 then 
       let ms = matchAllText regexp inp in
         (go 0 inp ms, (not . null) ms)
      else occur regexp inp repl n ""

matchRE pat str = str =~ pat :: Bool
