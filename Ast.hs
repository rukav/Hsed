module Ast where 
import SedRegex

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
            | ReadFile File             --  r file
            | Substitute  Pattern Replacement Flags  -- s/BRE/replacement/flags
            | Test (Maybe Label)        --  t [label]
            | WriteFile File            --  w file
            | Exchange                  --  x
            | Transform Text Text       --  y/string1/string2
            | Label String              --  :label
            | Comment                   --  #
            | EmptyCmd                  --  
    deriving Show

data PatternSpace = PatternSpace [String] deriving Show
data HoldSpace = HoldSpace [String] deriving Show
            
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
 
data Flags = Flags (Maybe OccurrencePrint) (Maybe File) 
    deriving Show

type Replacement = String 
type Invert = Bool
type Text = String
type Label = String
type File = String