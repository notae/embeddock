{-# OPTIONS -Wall #-}

import Control.Monad
import Data.List (isInfixOf)
import Data.String.Utils (replace)
import Language.Haskell.Her.HaLay (ready, tokssOut, toksOut, Tok(Com))
import System.Environment
import System.FilePath (splitFileName, splitExtension, (</>), (<.>))
import System.IO
import System.Process (runInteractiveCommand)
import Text.Printf


main :: IO ()
main = do
  (srcPath:_:destPath:ppOpts) <- getArgs
  src <- readFile srcPath
  let
    embedKey :: String
    runhaskellArgs :: [String]

    (embedKey,runhaskellArgs) = case ppOpts of
      []     -> ("$", [])
      (x:xs) -> (x, xs)

    openKey, closeKey :: String
    openKey = findFree "Open" 1 "Sesami"
    closeKey = findFree "Close" 1 "Sesami"

    findFree tag n tag' =
      let cand = tag ++ show (n::Integer) ++ tag' in
        if cand `isInfixOf` src then findFree tag (3*n) tag'
                                else cand

    parsedSrc :: [[Tok]]
    parsedSrc =
      map (filter (not . isEmbedPragma)) $
      ready srcPath src

    isEmbedPragma :: Tok -> Bool
    isEmbedPragma (Com str)
      | "OPTIONS_GHC" `isInfixOf` str && "embeddock" `isInfixOf` str
                    = True
      | otherwise   = False
    isEmbedPragma _ = False

    (srcDir, srcFn) = splitFileName srcPath
    (srcFnBody, srcExt) = splitExtension srcFn

    isEmbedLoop = (head srcFn == '.') &&
                  ("_embeddock." `isInfixOf` srcFn)

    runnerFn = srcDir </> ("." ++ srcFnBody ++ "_embeddock") <.> srcExt

    destContent = tokssOut parsedSrc

    quineMain = unlines $ "main = do" :map mkPrinter parsedSrc

    mkPrinter :: [Tok] -> String
    mkPrinter toks = ("  putStr $ " ++ ) $
        replace openKey  "\"++(" $
        replace closeKey ")++\"" $
        show $ toksOut $ map seedEmbed toks
      where
        embeds :: [String]
        embeds = toks >>= findEmbed

        seedEmbed :: Tok -> Tok
        seedEmbed (Com str) = Com $ foldl
            (\str' e -> replace (printf "%s(%s)" embedKey e)
                                (printf "%s%s%s" openKey e closeKey)
                                str')
            str embeds
        seedEmbed x = x


    findEmbed :: Tok -> [String]
    findEmbed (Com str) = go str
      where
        go []  = []
        go xss@(_:xs) = try embedKey xss `mplus` go xs

        try [] []       = []
        try _  []       = []
        try (k:ey) (x:xs)
          | k==x        = try ey xs
          | otherwise   = []
        try [] ('(':xs) = tryParen (1::Int) xs ""
        try _ _         = []

        tryParen n [] buf
          | n <= 0    = [reverse $ drop 1 buf]
          | otherwise = []

        tryParen n (x:xs) buf
          | n <= 0    = [reverse $ drop 1 buf]
          | otherwise = let next '(' = n+1
                            next ')' = n-1
                            next _   = n
                        in tryParen (next x) xs (x:buf)
    findEmbed _ = []



  when (not isEmbedLoop) $ do
    writeFile runnerFn $ destContent ++ "\n" ++ quineMain
    (_, hOut, _, _) <- runInteractiveCommand $
      printf "runhaskell %s %s" (unwords runhaskellArgs) runnerFn
    hGetContents hOut >>= writeFile destPath
