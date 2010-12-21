-- |
-- Module      :  SedRegex

-- Maintainer  :  virukav@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable-- |

-- The Sed regular expression implementation based on the regex-posix package.   

module SedRegex where

import Data.Array ((!))
import Data.Bits ((.|.))
import Text.Regex.Base (RegexMaker(makeRegexOpts),defaultExecOpt,
                        RegexLike(matchAll,matchAllText),RegexContext(matchM),MatchText)
import Text.Regex.Posix
--import Text.Regex.TDFA
import Text.Regex.Base.RegexLike
import Text.Regex
import qualified Data.ByteString.Char8 as B

type Pattern = B.ByteString

-- | Replaces every occurance of the given regexp with the replacement string. 
--   Modification of the subRegex function from regex-posix package.
sedSubRegex :: B.ByteString         -- ^ Search pattern
            -> B.ByteString         -- ^ Input string
            -> B.ByteString         -- ^ Replacement text
            -> Int                  -- ^ Occurrence
            -> (B.ByteString, Bool) -- ^ (Output string, Replacement occurs)
--sedSubRegex _ "" _ _ = ("", True)
sedSubRegex pat inp repl n =
  let regexp = makeRegexOpts compExtended defaultExecOpt pat
  --let regexp = mkRegex pat -- for TDFA
      compile _i str [] = \ _m -> B.append str
      compile i str ((xstr,(off,len)):rest) =
        let i' = off+len
            pre = B.take (off-i) str
            str' = B.drop (i'-i) str
            slashEsc = B.pack "\\"
            app m = if xstr == slashEsc then B.append slashEsc
                     else let x = read (B.unpack xstr) :: Int in 
                            B.append (fst (m!x))
        in if B.null str' then \ m -> B.append pre . app m
             else \ m -> B.append pre . app m . compile i' str' rest m

      compiled :: MatchText B.ByteString -> B.ByteString -> B.ByteString
      compiled = compile 0 repl findrefs where
        -- bre matches a backslash then capture either a backslash or some digits
        bre = mkRegex "\\\\(\\\\|[0-9]+)"
        findrefs = map (\m -> (fst (m!1),snd (m!0))) (matchAllText bre repl)
      go _i str [] = str
      go i str (m:ms) =
        let (_,(off,len)) = m!0
            i' = off+len
            pre = B.take (off-i) str
            str' = B.drop (i'-i) str
        in if B.null str' then B.concat [pre, compiled m B.empty]
             else B.concat [pre, compiled m (go i' str' ms)]
   
      occur regexp inp repl n pre =
        if inp == B.empty then (B.empty, False)
         else
          case matchOnceText regexp inp of
           Nothing -> (inp, False)
           Just (p, m, r) -> 
              if n > 1 then 
                 let acc = B.concat [pre, p, fst (m!0)] in
                     occur regexp r repl (n-1) acc
               else
                  (B.concat [pre, go 0 inp [m]], True)
  in if n == 0 then 
       let ms = matchAllText regexp inp in
         (go 0 inp ms, (not . null) ms)
      else occur regexp inp repl n B.empty

-- | Match the regular expression against text
matchRE pat str = str =~ pat :: Bool
