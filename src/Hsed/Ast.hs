-- |
-- Module      :  Ast
-- Copyright   :  (c) Vitaliy Rkavishnikov
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  virukav@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The main types used in the program

module Hsed.Ast where 

import Hsed.SedRegex (Pattern)
import Data.ByteString.Char8 (ByteString)

-- | Editing commands
data SedCmd = SedCmd Address SedFun  deriving Show

-- | Functions represents a single-character command verb
data SedFun = 
              Group [SedCmd]         -- ^ { - group of the sed commands
            | LineNum                -- ^ = - write to standard output the current line number
            | Append Text            -- ^ a - append text following each line matched by address 
            | Branch (Maybe Label)   -- ^ b - transfer control to Label
            | Change Text            -- ^ c - replace the lines selected by the address with Text
            | DeleteLine             -- ^ d - delete line(s) from pattern space
            | DeletePat              -- ^ D - delete (up to newline) of multiline pattern space
            | ReplacePat             -- ^ g - copy hold space into the pattern space
            | AppendPat              -- ^ G - add newline followed by hold space into the pattern space
            | ReplaceHold            -- ^ h - copy pattern space into hold space
            | AppendHold             -- ^ H - add newline followed by pattern space into the hold space
            | Insert Text            -- ^ i - insert Text before each line matched by address 
            | List                   -- ^ l - list the pattern space, showing non-printing chars in ASCII
            | NextLine               -- ^ n - read next line of input into pattern space
            | AppendLinePat          -- ^ N - add next input line and newline into pattern space
            | PrintPat               -- ^ p - print the addressed line(s)
            | WriteUpPat             -- ^ P - print (up to newline) of multiline pattern space   
            | Quit                   -- ^ q - quit when address is encounterd
            | ReadFile FilePath      -- ^ r - add contents of file to the pattern space
            | Substitute  Pattern Replacement Flags  -- ^ s - substitute Replacement for Pattern
            | Test (Maybe Label)     -- ^ t - branch to line marked by :label if substitution was made 
            | WriteFile FilePath     -- ^ w - write the line to file if a replacement was done
            | Exchange               -- ^ x - exchange pattern space with hold space
            | Transform Text Text    -- ^ y - transform each char by position in Text to Text
            | Label Label            -- ^ : - label a line in the scipt for transfering by b or t.
            | Comment                -- ^ # - ignore a line in the script except "#n" in the first line 
            | EmptyCmd               -- ^   - ignore spaces
    deriving Show

-- | An address is either a decimal number that counts input lines cumulatively across files, 
--   a '$' character that addresses the last line of input, or a context address as BRE 
data Addr = LineNumber Int
          | LastLine
          | Pat Pattern
    deriving Show

-- | A permissable address is representing by zero, one or two addresses
data Address = Address (Maybe Addr) (Maybe Addr) Invert
    deriving Show

-- | Used in the replacement string. An appersand ('&') will be replaced by the
--   string matched the BRE. The characters "\n", where n is a digit will be
--   replaced by the corresponding back-reference expression.
data Occurrence = Replace Int | ReplaceAll 
    deriving Show

-- | The flag to control the pattern space output in the substitute function
type OutputPat = Bool

-- | The allowed sequence of the Occurrence and OutputPat flags in the substitute
--   function
data OccurrencePrint = OccurrencePrint (Maybe Occurrence) OutputPat |
                       PrintOccurrence OutputPat (Maybe Occurrence)
    deriving Show

-- | Flags used in the substitute command
data Flags = Flags (Maybe OccurrencePrint) (Maybe FilePath) 
    deriving Show

type Replacement = ByteString
type Invert = Bool
type Text = ByteString
type Label = ByteString


