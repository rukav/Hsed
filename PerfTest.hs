import Criterion.Main
import qualified Control.Monad.State as S
import qualified StreamEd as E
import Prelude hiding (words)
import SedState

sysmonArgs = ["-n","-e","/Engine 0/p","tests/Perf/sysmon_100310_0952.out"]
wordsArgs = ["-n","-e","/^repay/p","tests/Perf/words.txt"]

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



main = defaultMain [
        bgroup "sed" [ bench "sysmon" $ whnfIO (sedBench sysmonArgs)
                     , bench "words" $ whnfIO (sedBench wordsArgs)
                     ]
                   ]
