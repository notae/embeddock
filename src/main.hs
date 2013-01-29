{-# OPTIONS -Wall #-}

import Language.Haskell.Her.HaLay (ready, tokssOut)
import Data.String.Utils (replace)
import System.Environment
import System.IO

main :: IO ()
main = do
  (fnsrc:_:fndest:_) <- getArgs
  src <- readFile fnsrc
  let parsedSrc = ready fnsrc $ replace "$(embed)" "kimney" src
  writeFile (fnsrc++".embeddock") $ tokssOut parsedSrc
  writeFile fndest $ tokssOut parsedSrc
