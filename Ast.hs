module Ast where 
import SedRegex
import Data.ByteString.Char8
--import qualified Data.ByteString.Lazy.Char8 as BS
data SedCmd = SedCmd Address SedFun  deriving Show

data SedFun = Group [SedCmd]
            | LineNum                   --  =
            | Append Text               --  a\<eol>text
            | Branch (Maybe Label)      --  b [label]
            | Change Text               --  c\<eol>text
            | Delete                    --  d
            | DeletePat                 --  D
            | ReplacePat                --  g
            | AppendPat                 --  G
            | ReplaceHold               --  h 
            | AppendHold                --  H
            | Insert Text               --  i\<eol>text
            | List                      --  l
            | Next                      --  n
            | AppendLinePat             --  N
            | PrintPat                  --  p
            | WriteUpPat                --  P    
            | Quit                      --  q
            | ReadFile FilePath         --  r file
            | Substitute  Pattern Replacement Flags  -- s/BRE/replacement/flags
            | Test (Maybe Label)        --  t [label]
            | WriteFile FilePath        --  w file
            | Exchange                  --  x
            | Transform Text Text       --  y/string1/string2
            | Label Label               --  :label
            | Comment                   --  #
            | EmptyCmd                  --  
    deriving Show

data PatternSpace = PatternSpace [ByteString] deriving Show
data HoldSpace = HoldSpace [ByteString] deriving Show
            
data Addr = LineNumber Int
          | LastLine
          | Pat Pattern
    deriving Show

data Address = Address (Maybe Addr) (Maybe Addr) Invert
    deriving Show

data Occurrence = Replace Int | ReplaceAll 
    deriving Show

type OutputPat = Bool
data OccurrencePrint = OccurrencePrint (Maybe Occurrence) OutputPat |
                       PrintOccurrence OutputPat (Maybe Occurrence)
    deriving Show
 
data Flags = Flags (Maybe OccurrencePrint) (Maybe FilePath) 
    deriving Show

type Replacement = ByteString
type Invert = Bool
type Text = ByteString
type Label = ByteString
