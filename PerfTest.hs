import System
import Criterion.Main
import qualified Control.Monad.State as S
import qualified StreamEd as E
import Prelude hiding (words)
import SedState

wordsArgs = ["-n","-e","/^repay/p","tests/words.txt"]

sedBench :: [String] -> IO ()
sedBench args = do
  S.execStateT ( 
    do 
      (files, cmds) <- E.parseArgs args
      set useMemSpace True
      E.compile cmds
      E.execute files
    ) initEnv
  return ()

main = do
  putStrLn "Native sed timing"
  system "time sed -n -e '/^ala/p' tests/words.txt > /dev/null"

  putStrLn "Hsed timing"
  system "time ./Hsed -n -e '/^ala/p' tests/words.txt > /dev/null"

  putStrLn "Hsed GHC report"
  system "./Hsed -n -e '/^ala/p' tests/words.txt > /dev/null +RTS -s"

  putStrLn "Hsed GHC build and profiling report"
  --system "ghc -O2 --make Hsed.hs"
  --system "ghc -O2 --make Hsed.hs -prof -auto-all -caf-all -fforce-recomp -osuf p_o -hisuf p_hi"
  system "./Hsed -n -e '/^ala/p' tests/words.txt > /dev/null +RTS -p"

  putStrLn "\nHsed Criterition report"
  defaultMain [bench "words" $ whnfIO (sedBench wordsArgs)]
